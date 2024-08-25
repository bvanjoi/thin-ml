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

export function isSameType(t1: Type, t2: Type): boolean {
	if (t1.type === 'TVar' && t2.type === 'TVar') {
		return isSameVariable(t1, t2)
	}
	if (t1.type === 'Int' && t2.type === 'Int') {
		return true
	}
	if (t1.type === 'Float' && t2.type === 'Float') {
		return true
	}
	if (t1.type === 'Bool' && t2.type === 'Bool') {
		return true
	}
	if (t1.type === 'TArr' && t2.type === 'TArr') {
		return isSameType(t1.ty1, t2.ty1) && isSameType(t1.ty2, t2.ty2)
	}
	return false
}

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
