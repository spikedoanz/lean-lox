import LeanLox.Core

def main (args : List String) : IO UInt32 := do
  match args.length with
  | 0 => runPrompt
  | 1 => 
    let filename := args[0]!
    runFile filename
  | _ => do
    IO.eprintln "Usage: lean-lox [script]"
    return 1
