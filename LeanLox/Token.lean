inductive TokenType where
  -- Single-character tokens
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  -- One or two character tokens
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  -- Literals
  | IDENTIFIER
  | STRING
  | NUMBER
  -- Keywords
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
  deriving Repr, BEq, Inhabited

def TokenType.toString : TokenType → String
  | LEFT_PAREN    => "LEFT_PAREN"
  | RIGHT_PAREN   => "RIGHT_PAREN"
  | LEFT_BRACE    => "LEFT_BRACE"
  | RIGHT_BRACE   => "RIGHT_BRACE"
  | COMMA         => "COMMA"
  | DOT           => "DOT"
  | MINUS         => "MINUS"
  | PLUS          => "PLUS"
  | SEMICOLON     => "SEMICOLON"
  | SLASH         => "SLASH"
  | STAR          => "STAR"
  | BANG          => "BANG"
  | BANG_EQUAL    => "BANG_EQUAL"
  | EQUAL         => "EQUAL"
  | EQUAL_EQUAL   => "EQUAL_EQUAL"
  | GREATER       => "GREATER"
  | GREATER_EQUAL => "GREATER_EQUAL"
  | LESS          => "LESS"
  | LESS_EQUAL    => "LESS_EQUAL"
  | IDENTIFIER    => "IDENTIFIER"
  | STRING        => "STRING"
  | NUMBER        => "NUMBER"
  | AND           => "AND"
  | CLASS         => "CLASS"
  | ELSE          => "ELSE"
  | FALSE         => "FALSE"
  | FUN           => "FUN"
  | FOR           => "FOR"
  | IF            => "IF"
  | NIL           => "NIL"
  | OR            => "OR"
  | PRINT         => "PRINT"
  | RETURN        => "RETURN"
  | SUPER         => "SUPER"
  | THIS          => "THIS"
  | TRUE          => "TRUE"
  | VAR           => "VAR"
  | WHILE         => "WHILE"
  | EOF           => "EOF"

instance : ToString TokenType where
  toString := TokenType.toString

structure Token where
  type : TokenType
  lexeme : String
  literal : Option String
  line : Nat
  deriving Repr, BEq

def makeToken (type : TokenType) (lexeme : String) (literal : Option String) (line : Nat) : Token :=
  { type := type, lexeme := lexeme, literal := literal, line := line }

def Token.toString (token : Token) : String :=
  match token.literal with
  | some lit => s!"{token.type} \t L{token.line} \t {token.lexeme} {lit}"
  | none =>     s!"{token.type} \t L{token.line} \t {token.lexeme} "
