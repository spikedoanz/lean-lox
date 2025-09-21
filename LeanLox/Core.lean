import LeanLox.Parser
import LeanLox.Scanner
import LeanLox.Interpreter

open LeanLox.Parser
open LeanLox.Interpreter

def indent (depth : Nat) : String :=
  String.join (List.replicate depth "  ")

def tPrint (depth : Nat) : Expr → String
  | Expr.Binary left op right => 
      let _ := indent depth
      let nextInd := indent (depth + 1)
      s!"({op.lexeme}\n{nextInd}{tPrint (depth + 1) left}\n{nextInd}{tPrint (depth + 1) right})"
  | Expr.Grouping expr => 
      s!"(group {tPrint depth expr})"
  | Expr.Literal (some value) => value
  | Expr.Literal none => "nil"
  | Expr.Unary op right => 
      s!"({op.lexeme} {tPrint depth right})"

def astPrint (expr : Expr) : String :=
  "==AST==\n" ++ (tPrint 0 expr) ++ "\n======="


def run (source : String) : IO Unit := do
  let tokens := scanTokens source
  match parse tokens with
  | Except.ok expr => 
      IO.println (astPrint expr)
      match evaluate expr with
      | .ok v => IO.println v
      | .error err => IO.println s!"Runtime error! {err}"
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
