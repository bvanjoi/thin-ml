Program = _ stmts:(stmt:Statment _ {return stmt})* _ {
    return {
      type: "Program",
      stmts,
    }
  }

Statment =
  ExpressionStmt
  / LetDecl

LetDecl
  = "let" _ recursion:"rec"? _ name:Identifier _ args:(_ arg: Identifier {return arg})* _ "=" _ expr:Expression _ ";" {
    if (args.length !== 0) {
      let rec = recursion != null;
      return { type: 'FunDecl', name, arguments: args, body: expr, rec }
    } else {
      return { type: "LetDecl", name, expr };
    }
  }

ExpressionStmt = expr:Expression _ ";" {
    return { type: 'ExprStmt', expr }
}

Expression
  = LetExpr 
  / IfExpr 
  / AppExpr   
  / BinaryExpr 
  / PrimaryExpr 
  / ParenthesesExpr

LetExpr
  = "let" _ name:Identifier _ params: "=" _ val:Expression _ "in" _ body:Expression {
    return { type: "LetExpr", name, val, body };
  }

IfExpr
  = "if" _ cond:Expression _ "then" _ then:Expression _ "else" _ e:Expression {
    if (typeof e === 'undefined') {
      return { type: "IfExpr", cond, then };
    } else {
      return { type: "IfExpr", cond, then, else: e };
    }
  }

AppExpr 
  = func:PrimaryExpr _ argument:Expression {
    return { type: "AppExpr", func, argument };
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
 / Bool

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
  / "false"
  / "true"
  / "rec"

Identifier
  = !keyword name:IdentifierName {  return { type: 'Ident', value: name } }

IdentifierName "identifier"
  = head:[a-zA-Z_] tail:[a-zA-Z0-9_]* {
      return head + tail.join("")
    }

Int = [0-9]+ { return { type: "IntLit", value: parseInt(text(), 10) }; }

Float = [0-9]+ "." [0-9]* { return { type: 'FloatLit', value: parseFloat(text())} }

Bool = BoolLit _ { return { type: "BoolLit", value: text() === "true" } }

BoolLit = "false" / "true"

Whitespace = " " / "\t" / "\n" / "\r"

_  = Whitespace*