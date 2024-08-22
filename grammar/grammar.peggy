Program
  = _ exprs:(expr:Expression _ ";" { return expr })* _ {
    return {
      type: "Program",
      exprs,
    }
  }

Expression
  = LetExpr 
  / IfExpr 
  / FunExpr 
  / AppExpr   
  / BinaryExpr 
  / PrimaryExpr 
  / ParenthesesExpr

LetExpr
  = "let" _ name:Identifier _ "=" _ val:Expression _ "in" _ body:Expression {
    return { type: "LetExpr", name, val, body };
  }

IfExpr
  = "if" _ cond:Expression _ "then" _ thenExpr:Expression (_ "else" _ elseExpr:Expression)* {
    if (typeof elseExpr === 'undefined') {
      return { type: "IfExpr", cond, thenExpr };
    } else {
      return { type: "IfExpr", cond, thenExpr, elseExpr };
    }
  }

FunExpr
  = "fun" _ param:Identifier _ "->" _ body:Expression {
    return { type: "FunExpr", param, body };
  }

AppExpr 
  = func:PrimaryExpr _ arg:PrimaryExpr {
    return { type: "AppExpr", func, arg };
  }

BinaryExpr = BinaryPrecOneExpr

BinaryPrecOneExpr = 
  left:BinaryPrecTwoExpr  _ 
  op: PrecOneOp _ 
  right:BinaryPrecOneExpr {
    return { type: "BinaryExpr", left, op, right };
  }
  / BinaryPrecTwoExpr

PrecOneOp = '+.' / '+' / '-.' / '-'

BinaryPrecTwoExpr =
  left:BinaryPrecThreeExpr _
  op: PrecTwoOp _
  right:BinaryPrecTwoExpr {
    return { type: "BinaryExpr", left, op, right };
  }
  / BinaryPrecThreeExpr

PrecTwoOp = "*." / "*" / "/." / "/"

BinaryPrecThreeExpr =
  left:PrimaryExpr _
  op: PrecThreeOp _
  right:Expression {
    return { type: "BinaryExpr", left, op, right };
  }
  / PrimaryExpr 

PrecThreeOp = "==" / '!=' / '<=' / '<' / '>=' / '>' 

PrimaryExpr
  = LiteralExpr
  / Identifier
  / ParenthesesExpr

LiteralExpr = Float
 / Int

ParenthesesExpr = "(" _ expr:Expression _ ")" { 
  return {
    type: 'ParenthesesExpr',
    expr,
  } 
}

keyword
  = "let"
  / "in"
  / "if"
  / "then"
  / "else"
  / "fun"

Identifier
  = !keyword [a-zA-Z_][a-zA-Z0-9_]* { return {type: 'Ident', name: text()}  }

Int = [0-9]+ { return { type: "IntLit", value: parseInt(text(), 10) }; }

Float = [0-9]+ "." [0-9]* { return { type: 'FloatLit', value: parseFloat(text())} }

Whitespace = " " / "\t" / "\n" / "\r"

_  = Whitespace*