import { test } from 'uvu'
import { equal, ok } from 'uvu/assert'
import {
	type Ambiguous,
	type InfiniteType,
	type UnboundVariable,
	UnificationFail,
	type UnificationMismatch,
} from '../src/errors'
import { exec } from '../src/index'
import type { Type } from '../src/type'

function shouldBeType(input: string, type: Type['type']) {
	const [tys, errors] = exec(input)
	equal(errors.length, 0)
	equal(tys.length, 1)
	equal(tys[0].type, type)
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

test('literal', () => {
	shouldBeType('4;', 'Int')
	shouldBeType('42;', 'Int')
	shouldBeType('42.;', 'Float')
	shouldBeType('42.42;', 'Float')
	shouldBeType('false;', 'Bool')
	shouldBeType('true;', 'Bool')
})
test('Binary', () => {
	shouldBeType('1 + 1;', 'Int')
	shouldBeType('1 - 1;', 'Int')
	shouldBeType('1 * 1;', 'Int')
	shouldBeType('1 / 1;', 'Int')
	shouldFailedWith('1 + 1.;', UnificationFail)
	shouldFailedWith('1 +. 1;', UnificationFail)
	shouldBeType('1. +. 1.;', 'Float')
	shouldBeType('1. -. 1.;', 'Float')
	shouldBeType('1. *. 1.;', 'Float')
	shouldBeType('1. /. 1.;', 'Float')
	shouldBeType('1 == 1;', 'Bool')
	shouldBeType('1. == 1.;', 'Bool')
	shouldBeType('1. == 1.;', 'Bool')
	shouldBeType('1 + 2 * 3;', 'Int')
	shouldBeType('1 * 2 + 3;', 'Int')
	shouldBeType('1 * (2 + 3);', 'Int')
	shouldBeType('(1 * 2) + 3;', 'Int')
})

test('Decl', () => {
	// shouldBeType('let a = 1;', 'Int')
})

test.run()
