import { test } from 'uvu'
import { equal, ok } from 'uvu/assert'
import {
	type Ambiguous,
	type InfiniteType,
	type UnboundVariable,
	UnificationFail,
	type UnificationMismatch,
} from '../src/errors'
import { exec, printTypeWithName } from '../src/index'
import type { Type } from '../src/type'

function shouldBeTypeForExpr(input: string, type: Type['type']) {
	const [tys, errors] = exec(input)
	equal(errors.length, 0)
	equal(tys.length, 1)
	equal(tys[0][1].type, type)
}
function shouldBeType(input: string, type: string) {
	const [tys, errors] = exec(input)
	equal(errors.length, 0)
	equal(tys.length, 1)
	equal(printTypeWithName(tys[0][0], tys[0][1]), type)
}
function lastType(input: string, type: Type['type']) {
	const [tys, errors] = exec(input)
	equal(errors.length, 0)
	ok(tys.length > 1)
	equal(tys[tys.length - 1][1].type, type)
}

function shouldFailedWith(
	input: string,
	e:
		| typeof UnificationFail
		| typeof InfiniteType
		| typeof UnboundVariable
		| typeof Ambiguous
		| typeof UnificationMismatch,
) {
	const [_, errors] = exec(input)
	equal(errors.length, 1)
	ok(errors[0] instanceof e)
}

test('type variable', () => {
	shouldBeType('let f x = x;', 'f : tVar(2) -> tVar(2)')
	shouldBeType('let f x = x + 1;', 'f : Int -> Int')
	lastType('let f x = x; f 1;', 'Int')
	lastType('let f x = x; f 1.;', 'Float')
	lastType('let f x = x; f false;', 'Bool')
	lastType('let f x = x; f true;', 'Bool')
	shouldFailedWith('let f x = x + 1; f 1.;', UnificationFail)
	lastType('let y = 1; let f x = x + y;', 'TArr')
	lastType('let y = 1; let f x = x + y; f 1;', 'Int')
	shouldBeType(
		'let rec fac n = if (n == 0) then 1 else (n * (fac (n-1)));',
		'fac : Int -> Int',
	)
})

test('literal', () => {
	shouldBeTypeForExpr('4;', 'Int')
	shouldBeTypeForExpr('42;', 'Int')
	shouldBeTypeForExpr('42.;', 'Float')
	shouldBeTypeForExpr('42.42;', 'Float')
	shouldBeTypeForExpr('false;', 'Bool')
	shouldBeTypeForExpr('true;', 'Bool')
})
test('Binary', () => {
	shouldBeTypeForExpr('1 + 1;', 'Int')
	shouldBeTypeForExpr('1 - 1;', 'Int')
	shouldBeTypeForExpr('1 * 1;', 'Int')
	shouldBeTypeForExpr('1 / 1;', 'Int')
	shouldFailedWith('1 + 1.;', UnificationFail)
	shouldFailedWith('1 +. 1;', UnificationFail)
	shouldBeTypeForExpr('1. +. 1.;', 'Float')
	shouldBeTypeForExpr('1. -. 1.;', 'Float')
	shouldBeTypeForExpr('1. *. 1.;', 'Float')
	shouldBeTypeForExpr('1. /. 1.;', 'Float')
	shouldBeTypeForExpr('1 == 1;', 'Bool')
	shouldBeTypeForExpr('1. == 1.;', 'Bool')
	shouldBeTypeForExpr('1. == 1.;', 'Bool')
	shouldBeTypeForExpr('1 + 2 * 3;', 'Int')
	shouldBeTypeForExpr('1 * 2 + 3;', 'Int')
	shouldBeTypeForExpr('1 * (2 + 3);', 'Int')
	shouldBeTypeForExpr('(1 * 2) + 3;', 'Int')
	lastType('let x = 1; x + x;', 'Int')
	lastType('let x = 1; x + 1;', 'Int')
	shouldFailedWith('let x = 1; x +. 1;', UnificationFail)
})
test('let Decl', () => {
	shouldBeType('let a = 1;', 'a : Int')
	shouldBeType('let a = false;', 'a : Bool')
	shouldBeType('let a = true;', 'a : Bool')
	shouldBeType('let a = 1.;', 'a : Float')
	lastType('let a = 1 + 1; a;', 'Int')
	shouldBeType('let a = if 1 == 0 then 1 else 2;', 'a : Int')
})

test.run()
