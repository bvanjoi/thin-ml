import { test } from 'uvu'
import { equal, throws } from 'uvu/assert'
import { type BinaryOp, parse } from '../grammar/parser'

function shouldParseSuccess(input: string) {
	const ast = parse(input)
	equal(ast.type, 'Program')
}

function shouldParseFail(input: string) {
	throws(() => parse(input))
}

function shouldType(
	input: string,
	type: 'IntLit' | 'FloatLit' | 'BoolLit' | 'Ident',
) {
	// console.log(JSON.stringify(parse(input), undefined, 2))
	const ast = parse(input)
	equal(ast.type, 'Program')
	equal(ast.stmts.length, 1)
	if (ast.stmts[0].type !== 'ExprStmt') {
		throw Error()
	}
	equal(ast.stmts[0].expr.type, type)
}

test('int literal', () => {
	const shouldIntLiteral = (input: string) => shouldType(input, 'IntLit')
	shouldIntLiteral('4;')
	shouldIntLiteral('42;')
})

test('float literal', () => {
	const shouldFloatLiteral = (input: string) => shouldType(input, 'FloatLit')

	shouldFloatLiteral('4.0;')
	shouldFloatLiteral('4.0 ;')
	shouldFloatLiteral('42.0;')
	shouldFloatLiteral('42.;')
	shouldParseFail('.42;')
})

test('bool literal', () => {
	const shouldBoolLiteral = (input: string) => shouldType(input, 'BoolLit')
	shouldBoolLiteral('true;')
	shouldBoolLiteral('false;')
	shouldBoolLiteral('false ;')
})

test('parentheses', () => {
	const ast = parse('(42);')
	equal(ast.type, 'Program')
	equal(ast.stmts.length, 1)
	if (ast.stmts[0].type !== 'ExprStmt') {
		throw new Error()
	}
	if (ast.stmts[0].expr.type !== 'ParenthesesExpr') {
		throw new Error()
	}
	equal(ast.stmts[0].expr.expr.type, 'IntLit')
})

test('identifier', () => {
	const shouldIdent = (input: string) => shouldType(input, 'Ident')
	// shouldIdent('foo;')
	// shouldIdent('x;')

	function is(input: string, name: string) {
		const ast = parse(input)
		equal(ast.type, 'Program')
		equal(ast.stmts.length, 1)
		if (
			ast.stmts[0].type === 'ExprStmt' &&
			ast.stmts[0].expr.type === 'Ident'
		) {
			equal(ast.stmts[0].expr.value, name)
		} else {
			throw Error()
		}
	}

	is('foo;', 'foo')
	is('abc;', 'abc')
	is('x;', 'x')

	const keywords = ['let', 'in', 'if', 'then', 'else', 'fun', 'false', 'true']
	for (const keyword of keywords) {
		shouldParseFail(`${keyword}`)
	}
})

test('binary expression', () => {
	function topOp(input: string, op: BinaryOp) {
		const ast = parse(input)
		equal(ast.type, 'Program')
		equal(ast.stmts.length, 1)
		if (ast.stmts[0].type !== 'ExprStmt') {
			throw new Error()
		}
		if (ast.stmts[0].expr.type !== 'BinaryExpr') {
			throw new Error()
		}
		equal(ast.stmts[0].expr.op, op)
	}

	topOp('x == y;', '==')
	topOp('1 + 2;', '+')
	topOp('1 +. 2;', '+.')
	topOp('1 - 2;', '-')
	topOp('1 -. 2;', '-.')
	topOp('1 * 2;', '*')
	topOp('1 *. 2;', '*.')
	topOp('1 / 2;', '/')
	topOp('1 /. 2;', '/.')
	topOp('1 == 2;', '==')
	topOp('1 != 2;', '!=')
	topOp('1 < 2;', '<')
	topOp('1 <= 2;', '<=')
	topOp('1 > 2;', '>')
	topOp('1 >= 2;', '>=')
	topOp('1 + 2 + 3;', '+')
	topOp('1 + 2 * 3;', '+')
	topOp('1 * 2 + 3;', '+')
	topOp('1 * (2 + 3);', '*')
	topOp('(1 * 2) + 3;', '+')
	topOp('(1 + 2) * 3;', '*')
})

test('if expression', () => {
	shouldParseSuccess('if 1 == 2 then 3 else 4;')
	shouldParseSuccess('if 1 == 2 then 3;')
})

test('let expression', () => {
	shouldParseSuccess('let x = 1 in x + 1;')
	shouldParseSuccess('let x = 1 in let y = 2 in x + y;')
})

function shouldDecl(input: string, decl: 'LetDecl' | 'FunDecl') {
	const ast = parse(input)
	equal(ast.type, 'Program')
	equal(ast.stmts.length, 1)
	equal(ast.stmts[0].type, decl)
}

test('let declaration', () => {
	shouldDecl('let x = 1;', 'LetDecl')
	shouldDecl('let f x y = x + y;', 'FunDecl')
	shouldDecl('let rec f n = if n == 0 then 1 else n * (f (n - 1));', 'FunDecl')

	function nameIs(input: string, name: string) {
		const ast = parse(input)
		equal(ast.type, 'Program')
		equal(ast.stmts.length, 1)
		if (ast.stmts[0].type === 'LetDecl') {
			equal(ast.stmts[0].name.value, name)
		} else if (ast.stmts[0].type === 'FunDecl') {
			equal(ast.stmts[0].name.value, name)
		} else {
			throw Error()
		}
	}
	nameIs('let x = 1;', 'x')
	nameIs('let rec f n = if n == 0 then 1 else n * (f (n - 1));', 'f')
	nameIs('let f x y = x + y;', 'f')
	nameIs('let abc = 1;', 'abc')
	nameIs('let abc x = x ;', 'abc')
	nameIs('let abc x y = x + y;', 'abc')
})

test('function application', () => {
	shouldParseSuccess('f;')
	shouldParseSuccess('f 1;')
	shouldParseSuccess('f 1 2;')
	shouldParseSuccess('f x y z;')
	shouldParseSuccess('f 1 2 3;')
	shouldParseSuccess('true 1;')
	shouldParseFail('let 1;')
})

test.run()
