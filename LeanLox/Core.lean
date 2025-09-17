import LeanLox.Scanner
import LeanLox.Parser

open LeanLox.Parser

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
  match parse tokens with
  | Except.ok expr => 
      IO.println (astPrint expr)
  | Except.error msg => 
      IO.eprintln s!"Parse error: {msg}"

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
