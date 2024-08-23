import { test } from 'uvu'
import { equal, throws } from 'uvu/assert'
import type { CheckTypeError } from '../src/errors'
import { exec } from '../src/index'
import type { Type } from '../src/type'

function shouldBeType(input: string, type: Type['type']) {
	const [tys, errors] = exec(input)
	equal(tys.length, 1)
	equal(tys[0].type, type)
}

function shouldFailedWith(input: string, e: CheckTypeError) {
	const tys = exec(input)
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
	shouldBeType('1 +. 1;', 'Int')
	shouldBeType('1 + 1;', 'Int')
	shouldBeType('1 + 1.;', 'Int')
})
test.run()
