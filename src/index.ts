import * as Immutable from 'immutable'
import {
	type BinaryOp,
	type Expression,
	type Program,
	type Statement,
	parse,
} from '../grammar/parser'
import type { Constraint } from './contraint'
import { UnboundVariable } from './errors'
import * as ty from './type'

type Name = string

function unreachable(): never {
	throw Error()
}

function isConstType(ty: ty.Type): boolean {
	return ty.type === 'Bool' || ty.type === 'Float' || ty.type === 'Int'
}

type AssumptionItem = [Name, ty.Type]

class Assumption {
	assumptions: Immutable.List<AssumptionItem>

	constructor(assumptions: Immutable.List<AssumptionItem>) {
		this.assumptions = assumptions
	}

	static empty(): Assumption {
		return new Assumption(Immutable.List())
	}

	extend(pair: AssumptionItem): Assumption {
		return new Assumption(Immutable.List([pair, ...this.assumptions]))
	}

	remove(varName: Name): Assumption {
		return new Assumption(this.assumptions.filter(([n, _]) => n !== varName))
	}

	lookup(key: Name): Immutable.List<ty.Type> {
		return this.assumptions.filter(([n, _]) => n === key).map(([_, t]) => t)
	}

	merge(other: Assumption): Assumption {
		return new Assumption(
			Immutable.List([...this.assumptions, ...other.assumptions]),
		)
	}

	static mergeAssumptions(assumptions: Assumption[]): Assumption {
		return assumptions.reduce(
			(acc, curr) => acc.merge(curr),
			Assumption.empty(),
		)
	}

	static singleton(pair: AssumptionItem): Assumption {
		return new Assumption(Immutable.List([pair]))
	}

	keys(): Immutable.List<Name> {
		return this.assumptions.map(([n, _]) => n)
	}
}

class TypeEnv {
	types: Immutable.Map<Name, ty.Scheme>
	constructor(types: Immutable.Map<Name, ty.Scheme>) {
		this.types = types
	}
	static empty(): TypeEnv {
		return new TypeEnv(Immutable.Map())
	}
	extend([x, s]: [Name, ty.Scheme]): TypeEnv {
		return new TypeEnv(this.types.set(x, s))
	}
	remove(varName: Name): TypeEnv {
		return new TypeEnv(this.types.delete(varName))
	}
	extends(xs: [Name, ty.Scheme][]): TypeEnv {
		return new TypeEnv(this.types.merge(Immutable.Map(xs)))
	}
	lookup(key: Name): ty.Scheme | undefined {
		return this.types.get(key)
	}
	merge(other: TypeEnv): TypeEnv {
		return new TypeEnv(this.types.merge(other.types))
	}
	static mergeEnvs(envs: TypeEnv[]): TypeEnv {
		return envs.reduce((acc, env) => acc.merge(env), TypeEnv.empty())
	}
	static singleton(x: Name, y: ty.Scheme): TypeEnv {
		return new TypeEnv(Immutable.Map([[x, y]]))
	}
	keys(): Name[] {
		return this.types.keySeq().toArray()
	}
	static fromList(xs: [Name, ty.Scheme][]): TypeEnv {
		return new TypeEnv(Immutable.Map(xs))
	}
	toList(): [Name, ty.Scheme][] {
		return this.types.entrySeq().toArray()
	}
}

type Subst = Immutable.Map<ty.TVar, ty.Type>

class Substitutable {
	subst: Subst
	constructor(subst: Subst) {
		this.subst = subst
	}

	substTVar(ty: ty.TVar): ty.TVar {
		const res = this.subst.get(ty)
		if (!res) {
			return ty
		}
		if (res.type === 'TVar') {
			return res
		}
		unreachable()
	}

	substType(ty: ty.Type): ty.Type {
		if (isConstType(ty)) {
			return ty
		}
		if (ty.type === 'TVar') {
			const res = this.subst.get(ty)
			if (!res) {
				return ty
			}
			return res
		}
		if (ty.type === 'TFunCall') {
			return {
				type: 'TFunCall',
				arguments: ty.arguments.map(arg => this.substType(arg)),
				ret: this.substType(ty.ret),
			}
		}
		unreachable()
	}

	substScheme(s: ty.Scheme): ty.Scheme {
		if (s.kind === 'ForAll') {
			const subst = this.subst.filter((_, v1) =>
				s.as.find(v2 => ty.isSameVariable(v1, v2)),
			)
			return {
				kind: 'ForAll',
				as: s.as,
				ty: new Substitutable(subst).substType(s.ty),
			}
		}
		unreachable()
	}

	substCons(c: Constraint): Constraint {
		if (c.kind === 'EqConst') {
			return {
				kind: 'EqConst',
				ty1: this.substType(c.ty1),
				ty2: this.substType(c.ty2),
			}
		}
		if (c.kind === 'ExpInstConst') {
			return {
				kind: 'ExpInstConst',
				ty: this.substType(c.ty),
				scheme: this.substScheme(c.scheme),
			}
		}
		if (c.kind === 'ImpInstConst') {
			return {
				kind: 'ImpInstConst',
				ty1: this.substType(c.ty1),
				ms: this.substSetVar(c.ms),
				ty2: this.substType(c.ty2),
			}
		}
		unreachable()
	}

	substListVar(v: Immutable.List<ty.TVar>): Immutable.List<ty.TVar> {
		return v.map(item => this.substTVar(item))
	}

	substSetVar(v: Immutable.Set<ty.TVar>): Immutable.Set<ty.TVar> {
		return v.map(item => this.substTVar(item))
	}
}

type Variables = Immutable.Set<ty.TVar>
// freeTypeVariables
function ftvTVar(t: ty.TVar): Variables {
	return Immutable.Set([t])
}

function ftvType(t: ty.Type): Variables {
	if (isConstType(t)) {
		return Immutable.Set()
	}
	if (t.type === 'TVar') {
		return ftvTVar(t)
	}
	if (t.type === 'TFunCall') {
		return Immutable.Set.union([
			...t.arguments.map(arg => ftvType(arg)),
			ftvType(t.ret),
		])
	}
	unreachable()
}
function ftvScheme(s: ty.Scheme): Variables {
	return ftvType(s.ty).subtract(s.as)
}
function ftvListVar(v: Immutable.List<ty.TVar>): Variables {
	return v.reduce<Variables>((prev, cur) => {
		return prev.union(ftvType(cur))
	}, Immutable.Set())
}
function ftvSetVar(v: Immutable.Set<ty.TVar>): Variables {
	return v.reduce<Variables>((prev, cur) => {
		return prev.union(ftvType(cur))
	}, Immutable.Set())
}
// ====

// activeTypeVariables
function atvConstraint(cons: Constraint): Variables {
	if (cons.kind === 'EqConst') {
		return Immutable.Set.union([ftvType(cons.ty1), ftvType(cons.ty2)])
	}
	if (cons.kind === 'ExpInstConst') {
		return Immutable.Set.union([ftvType(cons.ty), ftvScheme(cons.scheme)])
	}
	if (cons.kind === 'ImpInstConst') {
		return Immutable.Set.union([
			ftvType(cons.ty1),
			ftvSetVar(cons.ms).intersect(ftvType(cons.ty2)),
		])
	}

	unreachable()
}

function atvListConstraint(v: Immutable.List<Constraint>): Variables {
	return v.reduce<Variables>((prev, cur) => {
		return prev.union(atvConstraint(cur))
	}, Immutable.Set())
}

// ====
function fresh(): ty.TVar {
	const id = ty.tVarNextId()
	return { type: 'TVar', id }
}
function opType(op: BinaryOp): ty.Type {
	if (op === '*.' || op === '/.' || op === '+.' || op === '-.') {
		return {
			type: 'TArr',
			ty1: ty.typeFloat(),
			ty2: {
				type: 'TArr',
				ty1: ty.typeFloat(),
				ty2: ty.typeFloat(),
			},
		}
	}
	if (op === '*' || op === '/' || op === '+' || op === '-') {
		return {
			type: 'TArr',
			ty1: ty.typeInt(),
			ty2: {
				type: 'TArr',
				ty1: ty.typeInt(),
				ty2: ty.typeInt(),
			},
		}
	}
	const tv = fresh()
	return {
		type: 'TArr',
		ty1: tv,
		ty2: {
			type: 'TArr',
			ty1: tv,
			ty2: ty.typeBool(),
		},
	}
}

function inferExpr(expr: Expression): [Assumption, Constraint[], ty.Type] {
	if (expr.type === 'IntLit') {
		return [Assumption.empty(), [], ty.typeInt()]
	}
	if (expr.type === 'FloatLit') {
		return [Assumption.empty(), [], ty.typeFloat()]
	}
	if (expr.type === 'BoolLit') {
		return [Assumption.empty(), [], ty.typeBool()]
	}
	if (expr.type === 'Ident') {
		const tv = fresh()
		return [Assumption.empty(), [], tv]
	}
	if (expr.type === 'ParenthesesExpr') {
		return inferExpr(expr.expr)
	}
	if (expr.type === 'IfExpr') {
		const [as1, cs1, ty1] = inferExpr(expr.cond)
		const [as2, cs2, ty2] = inferExpr(expr.then)
		if (expr.else) {
			const [as3, cs3, ty3] = inferExpr(expr.then)
			return [
				Assumption.mergeAssumptions([as1, as2, as3]),
				[
					...cs1,
					...cs2,
					...cs3,
					{
						kind: 'EqConst',
						ty1: ty1,
						ty2: ty.typeBool(),
					},
					{
						kind: 'EqConst',
						ty1: ty2,
						ty2: ty3,
					},
				],
				ty2,
			]
		}
		return [
			Assumption.mergeAssumptions([as1, as2]),
			[
				...cs1,
				...cs2,
				{
					kind: 'EqConst',
					ty1: ty1,
					ty2: ty.typeBool(),
				},
			],
			ty2,
		]
	}
	if (expr.type === 'BinaryExpr') {
		const [as1, cs1, ty1] = inferExpr(expr.left)
		const [as2, cs2, ty2] = inferExpr(expr.right)
		const tv = fresh()
		const u1: ty.Type = {
			type: 'TArr',
			ty1,
			ty2: {
				type: 'TArr',
				ty1: ty2,
				ty2: tv,
			},
		}
		const u2 = opType(expr.op)
		return [
			Assumption.mergeAssumptions([as1, as2]),
			[...cs1, ...cs2, { kind: 'EqConst', ty1: u1, ty2: u2 }],
			tv,
		]
	}
	if (expr.type === 'LetExpr') {
	}
	if (expr.type === 'AppExpr') {
		const [as1, cs1, ty1] = inferExpr(expr.func)
		const [as2, cs2, ty2] = inferExpr(expr.argument)
		const tv = fresh()
		return [
			as1.merge(as2),
			[
				...cs1,
				...cs2,
				{
					kind: 'EqConst',
					ty1,
					ty2: {
						type: 'TArr',
						ty1: ty2,
						ty2: tv,
					},
				},
			],
			tv,
		]
	}
	unreachable()
}

function inferType(env: TypeEnv, expr: Expression) {
	const errors = []
	const [as, cs, t] = inferExpr(expr)
	const unbounds = Immutable.Set(as.keys()).subtract(env.keys())
	for (const v of unbounds) {
		errors.push(new UnboundVariable(v))
	}
	// const cs2 =
}

function inferStmt(env: TypeEnv, stmt: Statement): [TypeEnv, Error[]] {
	const tyEnv = env
	if (stmt.type === 'ExprStmt') {
	} else if (stmt.type === 'FnDecl') {
	} else if (stmt.type === 'LetDecl') {
	}
	unreachable()
}

function inferTop(env: TypeEnv, p: Program): [TypeEnv, Error[]] {
	let tyEnv = env
	const errors = []
	for (const stmt of p.stmts) {
		const res = inferStmt(tyEnv, stmt)
		tyEnv = res[0]
		errors.push(...res[1])
	}
	return [tyEnv, errors]
}

type Context = {
	ty: TypeEnv
}

export function exec(input: string): ty.Type[] {
	const p = parse(input)
	const [tyEnv, errors] = inferTop(TypeEnv.empty(), p)
	return []
}
