import type * as Immutable from 'immutable'
import type { Scheme, TVar, Type } from './type'

export type Constraint =
	| { kind: 'EqConst'; ty1: Type; ty2: Type }
	| { kind: 'ExpInstConst'; ty: Type; scheme: Scheme }
	| { kind: 'ImpInstConst'; ty1: Type; ms: Immutable.Set<TVar>; ty2: Type }
