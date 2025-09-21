import Batteries.Lean.HashMap
import LeanLox.Parser
open LeanLox.Parser

namespace LeanLox.Interpreter

def MAX_VALUES := 10000

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

structure Environment where
  values : Std.HashMap String Value
  enclosing : Option Environment
  deriving Repr, Inhabited

def Environment.empty : Environment :=
  { values := Std.HashMap.emptyWithCapacity MAX_VALUES,
    enclosing := none }

def Environment.withEnclosing (parent : Environment) : Environment :=
  { values := Std.HashMap.emptyWithCapacity MAX_VALUES,
    enclosing := some parent }

def Environment.define (env : Environment) (name: String) (value: Value) : Environment :=
  { env with values := env.values.insert name value }

partial def Environment.get (env : Environment) (name: String) : Except RuntimeError Value :=
  match env.values.get? name with
  | some v => pure v
  | none => 
    match env.enclosing with
    | some parent => parent.get name
    | none => .error (RuntimeError.unknownVariable name)

partial def Environment.assign (env : Environment) (name: String) (value: Value) 
    : Except RuntimeError Environment :=
  if env.values.contains name then
    .ok { env with values := env.values.insert name value }
  else
    match env.enclosing with
    | some parent => do
      let newParent ← parent.assign name value
      .ok { env with enclosing := some newParent }
    | none => .error (RuntimeError.unknownVariable name)

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

-- State for the interpreter
structure InterpreterState where
  env : Environment
  output : List String  -- To collect print output
  deriving Repr, Inhabited

abbrev InterpreterM := StateT InterpreterState (Except RuntimeError)

-- Forward declaration for mutual recursion
mutual

  partial def evaluateExpr (expr: Expr) : InterpreterM Value := do
    let state ← get
    match expr with
    | Expr.Literal lit => 
      pure (Value.fromLiteral lit)
      
    | Expr.Grouping e => 
      evaluateExpr e
      
    | Expr.Var name =>
      match state.env.get name.lexeme with
      | .ok v => pure v
      | .error e => throw e
      
    | Expr.Assign name value => do
      let v ← evaluateExpr value
      let state ← get
      match state.env.assign name.lexeme v with
      | .ok newEnv => do
        set { state with env := newEnv }
        pure v
      | .error e => throw e
      
    | Expr.Unary op right => do
      let rValue ← evaluateExpr right
      match op.type with
      | TokenType.MINUS => do
        match expectNumber rValue with
        | .ok n => pure (Value.number (-n))
        | .error e => throw e
      | TokenType.BANG => 
        pure (Value.boolean (not (Value.isTruthy rValue)))
      | _ => 
        throw (RuntimeError.unknownOperator s!"Unknown unary operator: {op.type}")
      
    | Expr.Binary left op right => do
      let lValue ← evaluateExpr left
      let rValue ← evaluateExpr right
      match op.type with
      | TokenType.MINUS => do
        match expectNumber lValue, expectNumber rValue with
        | .ok l, .ok r => pure (Value.number (l - r))
        | .error e, _ => throw e
        | _, .error e => throw e
      | TokenType.PLUS =>
        -- Handle both number addition and string concatenation
        match lValue, rValue with
        | Value.number l, Value.number r => pure (Value.number (l + r))
        | Value.string l, Value.string r => pure (Value.string (l ++ r))
        | Value.string l, Value.number r => pure (Value.string (l ++ r.toString))
        | _, _ => throw (RuntimeError.typeError 
            s!"Cannot add {lValue} and {rValue}")
      | TokenType.STAR => do
        match expectNumber lValue, expectNumber rValue with
        | .ok l, .ok r => pure (Value.number (l * r))
        | .error e, _ => throw e
        | _, .error e => throw e
      | TokenType.SLASH => do
        match expectNumber lValue, expectNumber rValue with
        | .ok l, .ok r => 
          if r == 0 then
            throw RuntimeError.divisionByZero
          else
            pure (Value.number (l / r))
        | .error e, _ => throw e
        | _, .error e => throw e
      | TokenType.GREATER => do
        match expectNumber lValue, expectNumber rValue with
        | .ok l, .ok r => pure (Value.boolean (l > r))
        | .error e, _ => throw e
        | _, .error e => throw e
      | TokenType.GREATER_EQUAL => do
        match expectNumber lValue, expectNumber rValue with
        | .ok l, .ok r => pure (Value.boolean (l >= r))
        | .error e, _ => throw e
        | _, .error e => throw e
      | TokenType.LESS => do
        match expectNumber lValue, expectNumber rValue with
        | .ok l, .ok r => pure (Value.boolean (l < r))
        | .error e, _ => throw e
        | _, .error e => throw e
      | TokenType.LESS_EQUAL => do
        match expectNumber lValue, expectNumber rValue with
        | .ok l, .ok r => pure (Value.boolean (l <= r))
        | .error e, _ => throw e
        | _, .error e => throw e
      | TokenType.EQUAL_EQUAL =>
        pure (Value.boolean (lValue == rValue))
      | TokenType.BANG_EQUAL =>
        pure (Value.boolean (lValue != rValue))
      | _ => 
        throw (RuntimeError.unknownOperator s!"Unknown binary operator: {op.type}")

  partial def executeStmt (stmt: Stmt) : InterpreterM Unit := do
    match stmt with
    | Stmt.Expression expr => do
      let _ ← evaluateExpr expr
      pure ()
      
    | Stmt.Print expr => do
      let value ← evaluateExpr expr
      let state ← get
      set { state with output := state.output ++ [toString value] }
      
    | Stmt.Var name initializer => do
      let value ← match initializer with
        | some init => evaluateExpr init
        | none => pure Value.nil
      let state ← get
      let newEnv := state.env.define name.lexeme value
      set { state with env := newEnv }
      
    | Stmt.Block stmts => do
      let state ← get
      let savedEnv := state.env
      let newEnv := Environment.withEnclosing savedEnv
      set { state with env := newEnv }
      executeStmts stmts
      -- Restore the previous environment
      let state ← get
      set { state with env := savedEnv }
  
  partial def executeStmts (stmts: List Stmt) : InterpreterM Unit := do
    match stmts with
    | [] => pure ()
    | stmt :: rest => do
      executeStmt stmt
      executeStmts rest

  partial def executeDecl (decl: Decl) : InterpreterM Unit := do
    match decl with
    | Decl.VarDecl name initializer => do
      let value ← match initializer with
        | some init => evaluateExpr init
        | none => pure Value.nil
      let state ← get
      let newEnv := state.env.define name.lexeme value
      set { state with env := newEnv }
      
    | Decl.Statement stmt =>
      executeStmt stmt

end

def interpret (program: Program) : Except RuntimeError (List String) := do
  let initialState : InterpreterState := 
    { env := Environment.empty, output := [] }
  
  let rec runProgram (decls: List Decl) : InterpreterM Unit := do
    match decls with
    | [] => pure ()
    | decl :: rest => do
      executeDecl decl
      runProgram rest
  
  match StateT.run (runProgram program) initialState with
  | .ok ((), finalState) => .ok finalState.output
  | .error e => .error e

end LeanLox.Interpreter
