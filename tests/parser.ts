import { test } from 'uvu'
import { equal, throws } from 'uvu/assert'
import { type BinaryOp, parse } from '../grammar/parser'

function shouldParseSuccess(input: string) {
	const ast = parse(input)
	equal(ast.type, 'Program')
}

function shouldType(input: string, type: 'IntLit' | 'FloatLit' | 'Ident') {
	// console.log(JSON.stringify(parse(input), undefined, 2))
	const ast = parse(input)
	equal(ast.type, 'Program')
	equal(ast.exprs.length, 1)
	equal(ast.exprs[0].type, type)
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
	throws(() => shouldFloatLiteral('.42'))
})

test('parentheses', () => {
	const ast = parse('(42);')
	equal(ast.type, 'Program')
	equal(ast.exprs.length, 1)
	if (ast.exprs[0].type !== 'ParenthesesExpr') {
		throw new Error('Expected ParenthesesExpr')
	}
	equal(ast.exprs[0].expr.type, 'IntLit')
})

test('identifier', () => {
	const shouldIdent = (input: string) => shouldType(input, 'Ident')
	shouldIdent('foo;')
	shouldIdent('x;')
	const keywords = ['let', 'in', 'if', 'then', 'else', 'fun']
	for (const keyword of keywords) {
		throws(() => shouldIdent(`${keyword};`))
	}
})

test('binary expression', () => {
	function topOp(input: string, op: BinaryOp) {
		const ast = parse(input)
		equal(ast.type, 'Program')
		equal(ast.exprs.length, 1)
		if (ast.exprs[0].type !== 'BinaryExpr') {
			throw new Error('Expected BinaryExpr')
		}
		equal(ast.exprs[0].op, op)
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

// test('function expression', () => {
// 	shouldParseSuccess('fun x -> x + 1;')
// 	shouldParseSuccess('fun x y z -> x + y + z;')
// 	shouldParseSuccess('fun x -> fun y + z -> x + y;')
// })

// test('function application', () => {
// 	shouldParseSuccess('(fun x -> x + 1) 2;')
// 	shouldParseSuccess('(fun x -> fun y -> x + y) 1 2;')
// })

test.run()
