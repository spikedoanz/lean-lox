import LeanLox.Token

namespace LeanLox.Parser

inductive Expr where
  | Binary    : Expr → Token → Expr → Expr
  | Grouping  : Expr → Expr
  | Literal   : Option String → Expr
  | Unary     : Token → Expr → Expr
  | Var       : Token → Expr 
  | Assign    : Token → Expr → Expr  -- Added for assignment support

inductive Stmt where
  | Expression : Expr → Stmt
  | Print      : Expr → Stmt
  | Var        : Token → Option Expr → Stmt  -- Variable declaration
  | Block      : List Stmt → Stmt

-- Declarations can be either variable declarations or statements
inductive Decl where
  | VarDecl : Token → Option Expr → Decl
  | Statement : Stmt → Decl

def Program := List Decl

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

def synchronize : Parser Unit := do
  let _ ← advance
  while true do
    let t ← peek
    if t.type == TokenType.EOF then
      break
    let prev ← previous
    if prev.type == TokenType.SEMICOLON then
      break
    match t.type with
    | TokenType.CLASS | TokenType.FUN | TokenType.VAR 
    | TokenType.FOR | TokenType.IF | TokenType.WHILE 
    | TokenType.PRINT | TokenType.RETURN => break
    | _ => let _ ← advance; pure ()

mutual
  partial def declaration : Parser Decl := do
    try
      if ← matchAny [TokenType.VAR] then
        varDeclaration
      else
        Decl.Statement <$> statement
    catch _ =>
      synchronize
      -- Return a dummy declaration to continue parsing
      pure (Decl.Statement (Stmt.Expression (Expr.Literal none)))

  partial def varDeclaration : Parser Decl := do
    let name ← consume TokenType.IDENTIFIER "Expect variable name."
    let initializer ← if ← matchAny [TokenType.EQUAL] then
      some <$> expression
    else
      pure none
    let _ ← consume TokenType.SEMICOLON "Expect ';' after variable declaration."
    pure (Decl.VarDecl name initializer)

  partial def statement : Parser Stmt := do
    if ← matchAny [TokenType.PRINT] then
      printStatement
    else if ← matchAny [TokenType.LEFT_BRACE] then
      Stmt.Block <$> block
    else
      expressionStatement

  partial def printStatement : Parser Stmt := do
    let value ← expression
    let _ ← consume TokenType.SEMICOLON "Expect ';' after value."
    pure (Stmt.Print value)

  partial def expressionStatement : Parser Stmt := do
    let expr ← expression
    let _ ← consume TokenType.SEMICOLON "Expect ';' after expression."
    pure (Stmt.Expression expr)

  partial def block : Parser (List Stmt) := do
    let mut statements := []
    while !(← check TokenType.RIGHT_BRACE) && !(← isAtEnd) do
      let decl ← declaration
      statements := statements ++ [declToStmt decl]
    let _ ← consume TokenType.RIGHT_BRACE "Expect '}' after block."
    pure statements
  where
    declToStmt : Decl → Stmt
    | Decl.VarDecl name init => Stmt.Var name init
    | Decl.Statement stmt => stmt

  partial def isAtEnd : Parser Bool := do
    let t ← peek
    pure (t.type == TokenType.EOF)

  partial def expression : Parser Expr := 
    assignment

  partial def assignment : Parser Expr := do
    let expr ← equality
    
    if ← matchAny [TokenType.EQUAL] then
      let equals ← previous
      let value ← assignment
      
      match expr with
      | Expr.Var name => pure (Expr.Assign name value)
      | _ => throw s!"Invalid assignment target at line {equals.line}"
    else
      pure expr

  partial def equality : Parser Expr := do
    let mut expr ← comparison
    while ← matchAny [TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL] do
      let op ← previous
      let right ← comparison
      expr := Expr.Binary expr op right
    pure expr

  partial def comparison : Parser Expr := do
    let mut expr ← term
    while ← matchAny [TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL] do
      let op ← previous
      let right ← term
      expr := Expr.Binary expr op right
    pure expr

  partial def term : Parser Expr := do
    let mut expr ← factor
    while ← matchAny [TokenType.MINUS, TokenType.PLUS] do
      let op ← previous
      let right ← factor
      expr := Expr.Binary expr op right
    pure expr

  partial def factor : Parser Expr := do
    let mut expr ← unary
    while ← matchAny [TokenType.SLASH, TokenType.STAR] do
      let op ← previous
      let right ← unary
      expr := Expr.Binary expr op right
    pure expr

  partial def unary : Parser Expr := do
    if ← matchAny [TokenType.BANG, TokenType.MINUS] then
      let op ← previous
      let right ← unary
      pure (Expr.Unary op right)
    else
      primary

  partial def primary : Parser Expr := do
    if ← matchAny [TokenType.FALSE] then
      pure (Expr.Literal (some "false"))
    else if ← matchAny [TokenType.TRUE] then
      pure (Expr.Literal (some "true"))
    else if ← matchAny [TokenType.NIL] then
      pure (Expr.Literal none)
    else if ← matchAny [TokenType.NUMBER, TokenType.STRING] then
      let t ← previous
      pure (Expr.Literal t.literal)
    else if ← matchAny [TokenType.IDENTIFIER] then
      let t ← previous
      pure (Expr.Var t)
    else if ← matchAny [TokenType.LEFT_PAREN] then
      let expr ← expression
      let _ ← consume TokenType.RIGHT_PAREN "Expect ')' after expression"
      pure (Expr.Grouping expr)
    else
      let t ← peek
      throw s!"Unexpected token '{t.lexeme}' at line {t.line}"
end

def parse (tokens : List Token) : Except String Program :=
  let initialState : ParserState := { tokens := tokens, current := 0 }
  let rec parseAll : Parser Program := do
    let mut decls := []
    while !(← isAtEnd) do
      let decl ← declaration
      decls := decls ++ [decl]
    pure decls
  match StateT.run parseAll initialState with
  | Except.ok (decls, _) => Except.ok decls
  | Except.error msg => Except.error msg
  where
    isAtEnd : Parser Bool := do
      let t ← peek
      pure (t.type == TokenType.EOF)

end Parser
