export type Program = {
	type: 'Program'
	exprs: Expression[]
}
export type Expression = ParenthesesExpr | BinaryExpr | PrimaryExpr

export type ParenthesesExpr = {
	type: 'ParenthesesExpr'
	expr: Expression
}

export type PrimaryExpr = LiteralExpr | Identifier

export type LiteralExpr = Int | Float

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
	binding: string
	body: Expression
}

export function parse(input: string): Program
