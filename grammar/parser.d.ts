export type Program = {
	type: 'Program'
	stmts: Statement[]
}
export type Statement = LetDecl | FnDecl | ExprStmt
export type LetDecl = {
	type: 'LetDecl'
	name: Identifier
	expr: Expression
}
export type FnDecl = {
	type: 'FunDecl'
	name: Identifier
	arguments: Identifier[]
	body: Expression
	rec: boolean
}
export type ExprStmt = {
	type: 'ExprStmt'
	expr: Expression
}
export type Expression =
	| ParenthesesExpr
	| BinaryExpr
	| PrimaryExpr
	| IfExpr
	| LetExpr
	| AppExpr

export type ParenthesesExpr = {
	type: 'ParenthesesExpr'
	expr: Expression
}

export type PrimaryExpr = LiteralExpr | Identifier

export type LiteralExpr = Int | Float | Bool

export type Identifier = {
	type: 'Ident'
	value: string
}
export type Int = {
	type: 'IntLit'
	value: number
}
export type Float = {
	type: 'FloatLit'
	value: number
}
export type Bool = {
	type: 'BoolLit'
	value: boolean
}
export type BinaryExpr = {
	type: 'BinaryExpr'
	op: BinaryOp
	left: Expression
	right: Expression
}
export type BinaryOp =
	| '+'
	| '+.'
	| '-'
	| '-.'
	| '*'
	| '*.'
	| '/'
	| '/.'
	| '=='
	| '!='
	| '<'
	| '<='
	| '>'
	| '>='

export type IfExpr = {
	type: 'IfExpr'
	cond: Expression
	then: Expression
	else: Expression | undefined
}

export type FunExpr = {
	type: 'FunExpr'
	param: Identifier
	body: Expression
}

export type LetExpr = {
	type: 'LetExpr'
	name: Identifier
	val: Expression
	body: Expression
}

export type AppExpr = {
	type: 'AppExpr'
	func: PrimaryExpr
	argument: Expression
}

export function parse(input: string): Program
