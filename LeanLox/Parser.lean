import LeanLox.Token

namespace LeanLox.Parser

inductive Expr where
  | Binary   : Expr → Token → Expr → Expr
  | Grouping : Expr → Expr
  | Literal  : Option String → Expr
  | Unary    : Token → Expr → Expr

structure ParserState where
  tokens : List Token
  current : Nat
  deriving Repr

abbrev Parser := StateT ParserState (Except String)

-- Helper functions
def peek : Parser Token := do
  let s ← get
  match s.tokens.drop s.current with
  | t :: _ => pure t
  | [] => pure { type := TokenType.EOF, lexeme := "", literal := none, line := 0 }

def previous : Parser Token := do
  let s ← get
  if s.current > 0 then
    match s.tokens[s.current - 1]? with
    | some t => pure t
    | none => throw "No previous token"
  else
    throw "No previous token"

def advance : Parser Token := do
  let t ← peek
  if t.type != TokenType.EOF then
    modify fun s => { s with current := s.current + 1 }
  pure t

def check (type : TokenType) : Parser Bool := do
  let t ← peek
  pure (t.type == type)

def matchAny (types : List TokenType) : Parser Bool := do
  for type in types do
    if ← check type then
      let _ ← advance
      return true
  return false

def consume (type : TokenType) (message : String) : Parser Token := do
  if ← check type then
    advance
  else
    let t ← peek
    throw s!"{message} at line {t.line}"

-- Mutual recursion block for expression parsing
mutual
  partial def expression : Parser Expr := 
    equality

  partial def equality : Parser Expr := do
    let mut expr ← comparison
    while ← matchAny [TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL] do
      let op ← previous
      let right ← comparison
      expr := .Binary expr op right
    pure expr

  partial def comparison : Parser Expr := do
    let mut expr ← term
    while ← matchAny [TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL] do
      let op ← previous
      let right ← term
      expr := .Binary expr op right
    pure expr

  partial def term : Parser Expr := do
    let mut expr ← factor
    while ← matchAny [TokenType.MINUS, TokenType.PLUS] do
      let op ← previous
      let right ← factor
      expr := .Binary expr op right
    pure expr

  partial def factor : Parser Expr := do
    let mut expr ← unary
    while ← matchAny [TokenType.SLASH, TokenType.STAR] do
      let op ← previous
      let right ← unary
      expr := .Binary expr op right
    pure expr

  partial def unary : Parser Expr := do
    if ← matchAny [TokenType.BANG, TokenType.MINUS] then
      let op ← previous
      let right ← unary
      pure (.Unary op right)
    else
      primary

  partial def primary : Parser Expr := do
    if ← matchAny [TokenType.FALSE] then
      pure (.Literal (some "false"))
    else if ← matchAny [TokenType.TRUE] then
      pure (.Literal (some "true"))
    else if ← matchAny [TokenType.NIL] then
      pure (.Literal none)
    else if ← matchAny [TokenType.NUMBER, TokenType.STRING] then
      let t ← previous
      pure (.Literal t.literal)
    else if ← matchAny [TokenType.LEFT_PAREN] then
      let expr ← expression
      let _ ← consume TokenType.RIGHT_PAREN "Expect ')' after expression"
      pure (.Grouping expr)
    else
      let t ← peek
      throw s!"Unexpected token '{t.lexeme}' at line {t.line}"
end

def parse (tokens : List Token) : Except String Expr :=
  let initialState : ParserState := { tokens := tokens, current := 0 }
  match StateT.run expression initialState with
  | Except.ok (expr, _) => Except.ok expr
  | Except.error msg => Except.error msg

end Parser
