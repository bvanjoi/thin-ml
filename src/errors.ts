import type { Constraint } from './contraint'
import type { TVar, Type } from './type'

export type CheckTypeError =
	| UnificationFail
	| InfiniteType
	| UnboundVariable
	| Ambiguous
	| UnificationMismatch

export class UnificationFail extends Error {
	constructor(type1: Type, type2: Type) {
		super(
			`Unification failed between ${JSON.stringify(type1)} and ${JSON.stringify(type2)}`,
		)
	}
}

export class InfiniteType extends Error {
	constructor(tvar: TVar, type: Type) {
		super(
			`Infinite type detected for variable ${tvar.id} with type ${JSON.stringify(type)}`,
		)
	}
}

export class UnboundVariable extends Error {
	constructor(variable: string) {
		super(`Unbound variable: ${variable}`)
	}
}

export class Ambiguous extends Error {
	constructor(constraints: Constraint[]) {
		super(`Ambiguous constraints: ${JSON.stringify(constraints)}`)
	}
}

export class UnificationMismatch extends Error {
	constructor(types1: Type[], types2: Type[]) {
		super(
			`Unification mismatch between ${JSON.stringify(types1)} and ${JSON.stringify(types2)}`,
		)
	}
}
