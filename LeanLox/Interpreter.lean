import LeanLox.Parser

open String
open LeanLox.Parser
namespace LeanLox.Interpreter

inductive RuntimeError where
  | typeError       : String → RuntimeError
  | divisionByZero  : RuntimeError
  | unknownVariable : String → RuntimeError
  | unknownOperator : String → RuntimeError
  deriving Repr, BEq

instance : ToString RuntimeError where
  toString
    | RuntimeError.divisionByZero       => "Division by zero"
    | RuntimeError.typeError msg        => s!"Type Error: {msg}"
    | RuntimeError.unknownVariable name => s!"Unknown variable '{name}'"
    | RuntimeError.unknownOperator msg  => s!"Runtime Error: {msg}"

inductive Value where
  | nil 
  | boolean (b: Bool) 
  | number (n: Float)
  | string (s: String)
  deriving Repr, BEq, Inhabited

instance : ToString Value where
  toString
    | Value.nil => "nil"
    | Value.boolean b => toString b
    | Value.number n => toString n
    | Value.string s => s

def parseNumber (s : String) : Option Float :=
  let chars := s.toList
  match chars.span (· != '.') with
  | (leftChars, []) => 
    -- No decimal point, parse as integer
    match leftChars.asString.toNat? with
    | some n => some (n.toFloat)
    | none => none
  | (leftChars, '.' :: rightChars) =>
    -- Has decimal point
    let leftStr := leftChars.asString
    let rightStr := rightChars.asString
    
    match leftStr.toNat? with
    | none => none
    | some leftInt =>
      let leftFloat := leftInt.toFloat
      if rightChars.isEmpty then
        -- Just "123." 
        some leftFloat
      else
        match rightStr.toNat? with
        | none => none
        | some rightInt =>
          let rightFloat := rightInt.toFloat
          let divisor := (10 ^ rightChars.length).toFloat
          some (leftFloat + rightFloat / divisor)
  | _ => none

def Value.fromLiteral (lit: Option String) : Value :=
  match lit with
  | none => Value.nil
  | some "true"   => Value.boolean true
  | some "false"  => Value.boolean false
  | some s =>
    match parseNumber s with
    | some n  => Value.number n
    | none    => Value.string s

def Value.isTruthy (v : Value) : Bool :=
  match v with
  | Value.nil => false
  | Value.boolean b => b
  | _ => true

def expectNumber (v : Value) : Except RuntimeError Float :=
  match v with
  | Value.number n => .ok n
  | _ => .error (RuntimeError.typeError s!"Expected number, got {v}")

def expectString (v : Value) : Except RuntimeError String :=
  match v with
  | Value.string s => .ok s
  | _ => .error (RuntimeError.typeError s!"Expected string, got {v}")

def evaluate (expr: Expr) : Except RuntimeError Value :=
  match expr with
  | Expr.Literal lit => 
    .ok (Value.fromLiteral lit)
    
  | Expr.Grouping e => 
    evaluate e
    
  | Expr.Unary op right => do
    let rValue ← evaluate right
    match op.type with
    | TokenType.MINUS => do
      let n ← expectNumber rValue
      .ok (Value.number (-n))
    | TokenType.BANG => 
      .ok (Value.boolean (not (Value.isTruthy rValue)))
    | _ => 
      .error (
      RuntimeError.unknownOperator s!"Unknown unary operator: {op.type}")
    
  | Expr.Binary left op right => do
    let lValue ← evaluate left
    let rValue ← evaluate right
    match op.type with
    | TokenType.MINUS => do
      let l ← expectNumber lValue
      let r ← expectNumber rValue
      .ok (Value.number (l - r))
    | TokenType.PLUS =>
      -- Handle both number addition and string concatenation
      match lValue, rValue with
      | Value.number l, Value.number r => .ok (Value.number (l + r))
      | Value.string l, Value.string r => .ok (Value.string (l ++ r))
      | Value.string l, Value.number r => .ok (Value.string (l ++ r.toString))
      | _, _ => .error (RuntimeError.typeError 
          s!"Cannot add {lValue} and {rValue}")
    | TokenType.STAR => do
      let l ← expectNumber lValue
      let r ← expectNumber rValue
      .ok (Value.number (l * r))
    | TokenType.SLASH => do
      let l ← expectNumber lValue
      let r ← expectNumber rValue
      if r == 0 then
        .error RuntimeError.divisionByZero
      else
        .ok (Value.number (l / r))
    | TokenType.GREATER => do
      let l ← expectNumber lValue
      let r ← expectNumber rValue
      .ok (Value.boolean (l > r))
    | TokenType.GREATER_EQUAL => do
      let l ← expectNumber lValue
      let r ← expectNumber rValue
      .ok (Value.boolean (l >= r))
    | TokenType.LESS => do
      let l ← expectNumber lValue
      let r ← expectNumber rValue
      .ok (Value.boolean (l < r))
    | TokenType.LESS_EQUAL => do
      let l ← expectNumber lValue
      let r ← expectNumber rValue
      .ok (Value.boolean (l <= r))
    | TokenType.EQUAL_EQUAL =>
      .ok (Value.boolean (lValue == rValue))
    | TokenType.BANG_EQUAL =>
      .ok (Value.boolean (lValue != rValue))
    | _ => 
      .error (RuntimeError.unknownOperator s!"Unknown binary operator: {op.type}")

end LeanLox.Interpreter
