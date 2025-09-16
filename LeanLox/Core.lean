import LeanLox.Token
import LeanLox.Scanner

inductive Expr where
  | Binary   : Expr → Token → Expr → Expr
  | Grouping : Expr → Expr
  | Literal  : Option String → Expr
  | Unary    : Token → Expr → Expr
  deriving Repr

def astPrint : Expr → String
  | Expr.Binary left op right => 
      s!"({op.lexeme} \n  {astPrint left} \n  {astPrint right}\n)"
  | Expr.Grouping expr => 
      s!"(group {astPrint expr})"
  | Expr.Literal (some value) => value
  | Expr.Literal none => "nil"
  | Expr.Unary op right => 
      s!"({op.lexeme} {astPrint right})"

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
