import LeanLox.Parser
import LeanLox.Scanner
import LeanLox.Interpreter

open LeanLox.Parser
open LeanLox.Interpreter

def indent (depth : Nat) : String :=
  String.join (List.replicate depth "  ")

-- Pretty print expressions for debugging
def tPrintExpr (depth : Nat) : Expr → String
  | Expr.Binary left op right => 
      let nextInd := indent (depth + 1)
      s!"({op.lexeme}\n{nextInd}{tPrintExpr (depth + 1) left}\n{nextInd}{tPrintExpr (depth + 1) right})"
  | Expr.Grouping expr => 
      s!"(group {tPrintExpr depth expr})"
  | Expr.Literal (some value) => value
  | Expr.Literal none => "nil"
  | Expr.Unary op right => 
      s!"({op.lexeme} {tPrintExpr depth right})"
  | Expr.Var name => s!"(var {name.lexeme})"
  | Expr.Assign name value => 
      s!"(assign {name.lexeme} {tPrintExpr depth value})"

-- Pretty print statements for debugging
def tPrintStmt (depth : Nat) : Stmt → String
  | Stmt.Expression expr => 
      s!"(expr-stmt {tPrintExpr depth expr})"
  | Stmt.Print expr => 
      s!"(print {tPrintExpr depth expr})"
  | Stmt.Var name init => 
      match init with
      | some expr => s!"(var {name.lexeme} = {tPrintExpr depth expr})"
      | none => s!"(var {name.lexeme})"
  | Stmt.Block stmts => 
      let stmtStrs := stmts.map (tPrintStmt (depth + 1))
      let indented := stmtStrs.map (fun s => indent (depth + 1) ++ s)
      s!"(block\n{String.intercalate "\n" indented})"

-- Pretty print declarations for debugging
def tPrintDecl (depth : Nat) : Decl → String
  | Decl.VarDecl name init => 
      match init with
      | some expr => s!"(var-decl {name.lexeme} = {tPrintExpr depth expr})"
      | none => s!"(var-decl {name.lexeme})"
  | Decl.Statement stmt => tPrintStmt depth stmt

def astPrint (program : Program) : String :=
  let declStrs := program.map (tPrintDecl 0)
  "==AST==\n" ++ String.intercalate "\n" declStrs ++ "\n======="

def run (source : String) : IO Unit := do
  let tokens := scanTokens source
  match parse tokens with
  | Except.ok program => 
      -- Optionally print AST for debugging
      -- IO.println (astPrint program)
      match interpret program with
      | .ok output => 
          -- Print each line of output from print statements
          for line in output do
            IO.println line
      | .error err => IO.println s!"Runtime error! {err}"
  | Except.error msg => 
      IO.eprintln s!"Parse error: {msg}"

partial def runPrompt : IO UInt32 := do
  IO.println "Lox REPL (type 'exit' to quit)"
  let stdin ← IO.getStdin
  let rec loop : IO UInt32 := do
    IO.print "> "
    let input ← stdin.getLine
    let trimmed := input.trim
    if trimmed == "exit" || trimmed == "quit" then
      IO.println "Goodbye!"
      return 0
    else if trimmed.length > 0 then
      run input
    loop
  loop

def runFile (filename: String) : IO UInt32 := do
  try
    let content ← IO.FS.readFile filename
    run content
    return 0
  catch e =>
    IO.eprintln s!"lean-lox: Error reading '{filename}': {e}"
    return 1

