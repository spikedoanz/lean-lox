import LeanLox.Core 

def testAst : IO Unit := do
  -- Create the expression: (* (- 123) (group 45.67))
  let expr := Expr.Binary
    (Expr.Unary
      (Token.mk TokenType.MINUS "-" none 1)
      (Expr.Literal (some "123")))
    (Token.mk TokenType.STAR "*" none 1)
    (Expr.Grouping
      (Expr.Literal (some "45.67")))
  
  IO.println (astPrint expr)
  
def main : IO Unit := do
  testAst
