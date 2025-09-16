import LeanLox.Token
open Token

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
  | _ :: rest => scanChars rest line acc -- skip unknown chars for now

def scanTokens (source : String) : List Token :=
  scanChars source.toList 1 []

def run (source : String) : IO Unit := do
  let tokens := scanTokens source
  for token in tokens do
    IO.println (Token.toString token)

partial def runPrompt : IO UInt32 := do
  IO.println "Lox REPL (type 'exit' to quit)"
  let stdin ← IO.getStdin
  let rec loop : IO UInt32 := do
    IO.print "> "
    let input ← stdin.getLine
    if input.trim == "exit" then
      return 0
    else
      run input
      loop
  loop

def runFile (filename: String) : IO UInt32 := do
  try
    let content ← IO.FS.readFile filename
    run content
    return 0
  catch e =>
    IO.eprintln s!"lean-lox {filename}: {e}"
    return 1

def main (args : List String) : IO UInt32 := do
  match args.length with
  | 0 => runPrompt
  | 1 => 
    let filename := args[0]!
    runFile filename
  | _ => do
    IO.eprintln "Usage: lean-lox [script]"
    return 1
