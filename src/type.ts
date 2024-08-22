import type * as Immutable from 'immutable'

let id = 0
export function tVarNextId(): number {
	const old = id
	id += 1
	return old
}

export function isSameVariable(v1: TVar, v2: TVar): boolean {
	return v1.id === v2.id
}

export type TVar = {
	type: 'TVar'
	id: number
}
export type TArr = {
	type: 'TArr'
	ty1: Type
	ty2: Type
}

export type Type =
	| TVar
	| { type: 'Int' }
	| { type: 'Float' }
	| { type: 'Bool' }
	| TArr

export const typeInt = (): Type => ({
	type: 'Int',
})
export const typeFloat = (): Type => ({
	type: 'Float',
})
export const typeBool = (): Type => ({
	type: 'Bool',
})

export type SchemeForAll = {
	kind: 'ForAll'
	as: Immutable.List<TVar>
	ty: Type
}
export type Scheme = SchemeForAll
