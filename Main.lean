import LeanLox.Core

def main (args : List String) : IO UInt32 := do
  match args with
  | [] => runPrompt
  | [filename] => runFile filename
  | _ => 
      IO.eprintln "Usage: lean-lox [script]"
      return 64
