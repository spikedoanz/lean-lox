import LeanLox.Token

partial def scanChars (chars : List Char) (line : Nat) (acc : List Token) : List Token :=
  match chars with
  | [] => (acc.reverse).append [{ type := .EOF, lexeme := "", literal := none, line := line }]
  | '(' :: rest => scanChars rest line ({ type := .LEFT_PAREN, lexeme := "(", literal := none, line := line } :: acc)
  | ')' :: rest => scanChars rest line ({ type := .RIGHT_PAREN, lexeme := ")", literal := none, line := line } :: acc)
  | '{' :: rest => scanChars rest line ({ type := .LEFT_BRACE, lexeme := "{", literal := none, line := line } :: acc)
  | '}' :: rest => scanChars rest line ({ type := .RIGHT_BRACE, lexeme := "}", literal := none, line := line } :: acc)
  | ',' :: rest => scanChars rest line ({ type := .COMMA, lexeme := ",", literal := none, line := line } :: acc)
  | '.' :: rest => scanChars rest line ({ type := .DOT, lexeme := ".", literal := none, line := line } :: acc)
  | '-' :: rest => scanChars rest line ({ type := .MINUS, lexeme := "-", literal := none, line := line } :: acc)
  | '+' :: rest => scanChars rest line ({ type := .PLUS, lexeme := "+", literal := none, line := line } :: acc)
  | ';' :: rest => scanChars rest line ({ type := .SEMICOLON, lexeme := ";", literal := none, line := line } :: acc)
  | '*' :: rest => scanChars rest line ({ type := .STAR, lexeme := "*", literal := none, line := line } :: acc)
  -- String literal
  | '"' :: rest =>
    let rec scanString (chars : List Char) (currentLine : Nat) (acc : List Char) : Option (String × List Char × Nat) :=
      match chars with
      | [] => none  -- Unterminated string
      | '"' :: rest => some (String.mk acc.reverse, rest, currentLine)
      | '\\' :: 'n' :: rest => scanString rest currentLine ('\n' :: acc)  -- Newline escape
      | '\\' :: 't' :: rest => scanString rest currentLine ('\t' :: acc)  -- Tab escape
      | '\\' :: 'r' :: rest => scanString rest currentLine ('\r' :: acc)  -- Carriage return
      | '\\' :: '\\' :: rest => scanString rest currentLine ('\\' :: acc)  -- Backslash escape
      | '\\' :: '"' :: rest => scanString rest currentLine ('"' :: acc)   -- Quote escape
      | '\n' :: rest => scanString rest (currentLine + 1) ('\n' :: acc)   -- Track line numbers
      | c :: rest => scanString rest currentLine (c :: acc)
    match scanString rest line [] with
    | some (str, remaining, newLine) => 
        scanChars remaining newLine ({ type := .STRING, lexeme := "\"" ++ str ++ "\"", literal := some str, line := line } :: acc)
    | none => 
        -- Unterminated string - for now just skip to end
        scanChars [] line acc
  -- Single-line comment: skip until \n  
  | '/' :: '/' :: rest => 
    let rec skipToNewline (chars : List Char) : List Char :=
      match chars with
      | [] => []
      | '\n' :: rest => '\n' :: rest  -- Keep the newline to process it normally
      | _ :: rest => skipToNewline rest
    scanChars (skipToNewline rest) line acc
  -- Multi line comment: skip until */
  | '/' :: '*' :: rest =>
    let rec skipToCommentClose (chars : List Char) (currentLine : Nat) : (List Char × Nat) :=
      match chars with
      | [] => ([], currentLine)
      | '*' :: '/' :: rest => (rest, currentLine)
      | '\n' :: rest => skipToCommentClose rest (currentLine + 1) -- accurate line tracking
      | _ :: rest => skipToCommentClose rest currentLine
    let (remaining, newLine) := skipToCommentClose rest line
    scanChars remaining newLine acc
  | '/' :: rest => scanChars rest line ({ type := .SLASH, lexeme := "/", literal := none, line := line } :: acc)
  | '!' :: '=' :: rest => scanChars rest line ({ type := .BANG_EQUAL, lexeme := "!=", literal := none, line := line } :: acc)
  | '!' :: rest => scanChars rest line ({ type := .BANG, lexeme := "!", literal := none, line := line } :: acc)
  | '=' :: '=' :: rest => scanChars rest line ({ type := .EQUAL_EQUAL, lexeme := "==", literal := none, line := line } :: acc)
  | '=' :: rest => scanChars rest line ({ type := .EQUAL, lexeme := "=", literal := none, line := line } :: acc)
  | '>' :: '=' :: rest => scanChars rest line ({ type := .GREATER_EQUAL, lexeme := ">=", literal := none, line := line } :: acc)
  | '>' :: rest => scanChars rest line ({ type := .GREATER, lexeme := ">", literal := none, line := line } :: acc)
  | '<' :: '=' :: rest => scanChars rest line ({ type := .LESS_EQUAL, lexeme := "<=", literal := none, line := line } :: acc)
  | '<' :: rest => scanChars rest line ({ type := .LESS, lexeme := "<", literal := none, line := line } :: acc)
  | '\n' :: rest => scanChars rest (line + 1) acc
  | ' ' :: rest | '\t' :: rest | '\r' :: rest => scanChars rest line acc
  | c :: rest =>
    if c.isDigit then
      -- Number literal
      let rec scanDigits (chars : List Char) (digits : List Char) : (List Char × List Char) :=
        match chars with
        | c :: rest => if c.isDigit then scanDigits rest (c :: digits) else (chars, digits)
        | [] => ([], digits)
      let (afterInt, intDigits) := scanDigits rest [c]
      -- Check for decimal point followed by digits
      match afterInt with
      | '.' :: d :: rest2 =>
        if d.isDigit then
          -- Decimal number
          let (remaining, fracDigits) := scanDigits rest2 [d]
          let numberStr := String.mk (intDigits.reverse ++ ['.'] ++ fracDigits.reverse)
          scanChars remaining line ({ type := .NUMBER, lexeme := numberStr, literal := some numberStr, line := line } :: acc)
        else
          -- Just an integer followed by a dot (not a decimal number)
          let numberStr := String.mk intDigits.reverse
          scanChars afterInt line ({ type := .NUMBER, lexeme := numberStr, literal := some numberStr, line := line } :: acc)
      | _ =>
        -- Integer only
        let numberStr := String.mk intDigits.reverse
        scanChars afterInt line ({ type := .NUMBER, lexeme := numberStr, literal := some numberStr, line := line } :: acc)
    else if c.isAlpha || c == '_' then
      -- Identifier or keyword
      let rec scanIdentifier (chars : List Char) (ident : List Char) : (List Char × List Char) :=
        match chars with
        | c :: rest => 
          if c.isAlpha || c.isDigit || c == '_' then 
            scanIdentifier rest (c :: ident) 
          else 
            (chars, ident)
        | [] => ([], ident)
      let (remaining, identChars) := scanIdentifier rest [c]
      let identStr := String.mk identChars.reverse
      -- Check if it's a keyword
      let tokenType := match identStr with
        | "and" => TokenType.AND
        | "class" => TokenType.CLASS
        | "else" => TokenType.ELSE
        | "false" => TokenType.FALSE
        | "for" => TokenType.FOR
        | "fun" => TokenType.FUN
        | "if" => TokenType.IF
        | "nil" => TokenType.NIL
        | "or" => TokenType.OR
        | "print" => TokenType.PRINT
        | "return" => TokenType.RETURN
        | "super" => TokenType.SUPER
        | "this" => TokenType.THIS
        | "true" => TokenType.TRUE
        | "var" => TokenType.VAR
        | "while" => TokenType.WHILE
        | _ => TokenType.IDENTIFIER
      -- For literals like true/false/nil, store their value
      let literal := match identStr with
        | "true" => some "true"
        | "false" => some "false"
        | "nil" => some "nil"
        | _ => none
      scanChars remaining line ({ type := tokenType, lexeme := identStr, literal := literal, line := line } :: acc)
    else
      -- Skip unknown characters for now
      scanChars rest line acc

def scanTokens (source : String) : List Token :=
  scanChars source.toList 1 []
