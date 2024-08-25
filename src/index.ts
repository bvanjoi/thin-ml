import * as Immutable from 'immutable'
import {
	type BinaryOp,
	type Expression,
	type Program,
	type Statement,
	parse,
} from '../grammar/parser'
import type { Constraint } from './constraint'
import {
	InfiniteType,
	UnboundVariable,
	UnificationFail,
	UnificationMismatch,
} from './errors'
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
		if (ty.type === 'TArr') {
			return {
				type: 'TArr',
				ty1: this.substType(ty.ty1),
				ty2: this.substType(ty.ty2),
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

	substListCons(v: Immutable.List<Constraint>): Immutable.List<Constraint> {
		return v.map(item => this.substCons(item))
	}

	substListVar(v: Immutable.List<ty.TVar>): Immutable.List<ty.TVar> {
		return v.map(item => this.substTVar(item))
	}

	substListType(v: Immutable.List<ty.Type>): Immutable.List<ty.Type> {
		return v.map(item => this.substType(item))
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
	if (t.type === 'TArr') {
		return Immutable.Set.union([ftvType(t.ty1), ftvType(t.ty2)])
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

type VariableWithinLetInScope = Immutable.Set<ty.TVar>

function inferExpr(
	expr: Expression,
	v: VariableWithinLetInScope,
): [Assumption, Constraint[], ty.Type] {
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
		return [Assumption.singleton([expr.value, tv]), [], tv]
	}
	if (expr.type === 'ParenthesesExpr') {
		return inferExpr(expr.expr, v)
	}
	if (expr.type === 'IfExpr') {
		const [as1, cs1, ty1] = inferExpr(expr.cond, v)
		const [as2, cs2, ty2] = inferExpr(expr.then, v)
		if (expr.else) {
			const [as3, cs3, ty3] = inferExpr(expr.then, v)
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
				// TODO: eqConst between ty2 and unit
			],
			ty2,
		]
	}
	if (expr.type === 'BinaryExpr') {
		const [as1, cs1, ty1] = inferExpr(expr.left, v)
		const [as2, cs2, ty2] = inferExpr(expr.right, v)
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
		const [as1, cs1, ty1] = inferExpr(expr.val, v)
		const [as2, cs2, ty2] = inferExpr(expr.body, v)
		return [
			as1.merge(as2),
			[
				...cs1,
				...cs2,
				...as2.lookup(expr.name.value).map(
					ty =>
						({
							kind: 'ImpInstConst',
							ty1: ty,
							ms: v,
							ty2: ty1,
						}) as Constraint,
				),
			],
			ty2,
		]
	}
	if (expr.type === 'AppExpr') {
		const [as1, cs1, ty1] = inferExpr(expr.func, v)
		const [as2, cs2, ty2] = inferExpr(expr.argument, v)
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

function inferFix(
	v: Lambda,
	vars: VariableWithinLetInScope,
): [[Assumption, Constraint[], ty.Type], VariableWithinLetInScope] {
	const [[as, cs, t], vars2] = inferLambda(v, vars)
	const tv = fresh()
	return [
		[
			as,
			[
				...cs,
				{
					kind: 'EqConst',
					ty1: t,
					ty2: {
						type: 'TArr',
						ty1: tv,
						ty2: tv,
					},
				},
			],
			tv,
		],
		vars2,
	]
}

function inferLambda(
	v: Lambda,
	vars: VariableWithinLetInScope,
): [[Assumption, Constraint[], ty.Type], VariableWithinLetInScope] {
	const a = fresh()
	let as: Assumption
	let cs: Constraint[]
	let t: ty.Type
	let vars2 = vars

	if (v.body.type === 'Lambda') {
		// TODO: insert a into inferState
		;[[as, cs, t], vars2] = inferLambda(v.body, vars)
		vars2 = vars.add(a)
	} else {
		;[as, cs, t] = inferExpr(v.body, vars)
	}

	return [
		[
			as.remove(v.name),
			[
				...cs,
				...as
					.lookup(v.name)
					.map(v => ({ kind: 'EqConst', ty1: v, ty2: a }) as Constraint),
			],
			{ type: 'TArr', ty1: a, ty2: t },
		],
		vars2,
	]
}

function inferType(
	env: TypeEnv,
	[as, cs, t]: [Assumption, Constraint[], ty.Type],
): [Subst, ty.Type] {
	const errors = []
	const unbounds = Immutable.Set(as.keys()).subtract(env.keys())
	//  Immutable.Set(env.keys()).subtract(as.keys())
	for (const unbound of unbounds) {
		errors.push(new UnboundVariable(unbound))
		throw Error(errors.join(','))
	}
	const cs2: Constraint[] = []
	for (const [x, scheme] of env.toList()) {
		cs2.push(
			...as
				.lookup(x)
				.map(ty => ({ kind: 'ExpInstConst', ty, scheme }) as Constraint),
		)
	}

	const subst = solve([...cs2, ...cs])
	return [subst, new Substitutable(subst).substType(t)]
}

function normalize(s: ty.Scheme): ty.Scheme {
	if (s.kind === 'ForAll') {
		function normtype(t: ty.Type): ty.Type {
			if (isConstType(t)) {
				return t
			}
			if (t.type === 'TVar') {
				const res = ord.find(item => ty.isSameVariable(item[0], t))
				if (!res) {
					throw Error('type variable not in signature')
				}
				return res[1]
			}
			if (t.type === 'TArr') {
				return {
					type: 'TArr',
					ty1: normtype(t.ty1),
					ty2: normtype(t.ty2),
				}
			}
			unreachable()
		}

		function fv(t: ty.Type): ty.Type[] {
			if (isConstType(t)) {
				return []
			}
			if (t.type === 'TVar') {
				return [t]
			}
			if (t.type === 'TArr') {
				return [...fv(t.ty1), ...fv(t.ty2)]
			}
			unreachable()
		}

		const ord = Array.from(new Set(fv(s.ty))).map(
			item => [item, fresh()] as [ty.TVar, ty.TVar],
		)
		return {
			kind: 'ForAll',
			as: Immutable.List(ord.map(([_, snd]) => snd)),
			ty: normtype(s.ty),
		}
	}
	unreachable()
}

function closeOver(ty: ty.Type): ty.Scheme {
	return normalize(generalize(Immutable.Set(), ty))
}

let exprCount = 0
const EXPR_NAME = 'it'
function nextExprId() {
	const old = exprCount
	exprCount += 1
	return `${EXPR_NAME}${old}`
}

interface Lambda {
	type: 'Lambda'
	name: string
	body: Lambda | Expression
}

function inferStmt(env: TypeEnv, stmt: Statement): TypeEnv {
	const tyEnv = env
	let varsWithinLetInScope: VariableWithinLetInScope = Immutable.Set()
	if (stmt.type === 'ExprStmt') {
		const [subst, ty] = inferType(
			tyEnv,
			inferExpr(stmt.expr, varsWithinLetInScope),
		)
		return tyEnv.extend([
			nextExprId(),
			closeOver(new Substitutable(subst).substType(ty)),
		])
	}

	if (stmt.type === 'FunDecl') {
		function foldR(body: Expression, args: string[]): Lambda {
			let before: Expression | Lambda = body
			for (let i = args.length - 1; i >= 0; i -= 1) {
				before = {
					type: 'Lambda',
					name: args[i],
					body: before,
				} as Lambda
			}
			if (before.type !== 'Lambda') {
				throw Error('maybe arguments is empty which means parse had bug')
			}
			return before
		}

		const lambda = foldR(
			stmt.body,
			stmt.arguments.map(i => i.value),
		)
		if (stmt.rec) {
			const [a, b] = inferLambda(lambda, varsWithinLetInScope)
			varsWithinLetInScope = b
			const [subst, ty] = inferType(tyEnv, a)
			return tyEnv.extend([
				stmt.name.value,
				closeOver(new Substitutable(subst).substType(ty)),
			])
		}
		const [a, b] = inferLambda(lambda, varsWithinLetInScope)
		varsWithinLetInScope = b
		const [subst, ty] = inferType(tyEnv, a)
		return tyEnv.extend([
			stmt.name.value,
			closeOver(new Substitutable(subst).substType(ty)),
		])
	}

	if (stmt.type === 'LetDecl') {
		const tv = fresh()
		const as1 = Assumption.singleton([stmt.name.value, tv])
		const [as2, cs2, ty2] = inferExpr(stmt.expr, varsWithinLetInScope)
		const [subst, ty] = inferType(tyEnv, [
			as1.merge(as2).remove(stmt.name.value),
			[
				...cs2,
				{
					kind: 'EqConst',
					ty1: tv,
					ty2: ty2,
				},
			],
			tv,
		])
		return tyEnv.extend([
			stmt.name.value,
			closeOver(new Substitutable(subst).substType(ty)),
		])
	}
	unreachable()
}

function inferTop(env: TypeEnv, p: Program): [TypeEnv, TypeError[]] {
	let tyEnv = env
	const errors: TypeError[] = []
	for (const stmt of p.stmts) {
		try {
			tyEnv = inferStmt(tyEnv, stmt)
		} catch (error: unknown) {
			errors.push(error as TypeError)
		}
	}
	return [tyEnv, errors]
}

function nextSolvable(cs: Constraint[]): [Constraint, Constraint[]] {
	function chooseOne(cs: Constraint[]): [Constraint, Constraint[]][] {
		return cs.map((x, i) => {
			const ys = cs.filter((_, j) => i !== j)
			return [x, ys]
		})
	}
	function solvable(c: Constraint): boolean {
		if (c.kind === 'EqConst' || c.kind === 'ExpInstConst') {
			return true
		}
		return (
			Immutable.Set.intersect([ftvType(c.ty2).subtract(c.ms), atvConstraint(c)])
				.size === 0
		)
	}
	const res = chooseOne(cs).find(([c]) => solvable(c))
	if (!res) {
		throw Error('No solvable constraint')
	}
	return res
}

function bind(v: ty.TVar, t: ty.Type): Subst {
	if (t.type === 'TVar' && ty.isSameVariable(v, t)) {
		return Immutable.Map()
	}
	if (ftvType(t).has(v)) {
		throw new InfiniteType(v, t)
	}
	return Immutable.Map([[v, t]])
}

function compose(s1: Subst, s2: Subst): Subst {
	const s = s2.map(t => new Substitutable(s1).substType(t))
	return s.merge(s1)
}

function unifyMany(t1: ty.Type[], t2: ty.Type[]): Subst {
	if (t1.length === 0 && t2.length === 0) {
		return Immutable.Map()
	}
	if (t1.length === 0 || t2.length === 0) {
		throw new UnificationMismatch(t1, t2)
	}
	const [t1Head, ...t1Tail] = t1
	const [t2Head, ...t2Tail] = t2
	const su1 = unifies(t1Head, t2Head)
	const su2 = unifyMany(
		new Substitutable(su1).substListType(Immutable.List(t1Tail)).toArray(),
		new Substitutable(su1).substListType(Immutable.List(t2Tail)).toArray(),
	)
	return compose(su2, su1)
}

function unifies(t1: ty.Type, t2: ty.Type): Subst {
	if (ty.isSameType(t1, t2)) {
		return Immutable.Map()
	}
	if (t1.type === 'TVar') {
		return bind(t1, t2)
	}
	if (t2.type === 'TVar') {
		return bind(t2, t1)
	}
	if (t1.type === 'TArr' && t2.type === 'TArr') {
		return unifyMany([t1.ty1, t1.ty2], [t2.ty1, t2.ty2])
	}
	throw new UnificationFail(t1, t2)
}

function generalize(free: Immutable.Set<ty.TVar>, ty: ty.Type): ty.Scheme {
	const bound = ftvType(ty).subtract(free)
	return {
		kind: 'ForAll',
		as: bound.toList(),
		ty,
	}
}

function instantiate(s: ty.Scheme): ty.Type {
	const as = s.as.map(fresh)
	const su = Immutable.Map(s.as.map((v, i) => [v, as.get(i) as ty.Type]))
	return new Substitutable(su).substType(s.ty)
}

function solve(cs: Constraint[]): Subst {
	if (cs.length === 0) {
		return Immutable.Map()
	}
	const [nextC, nextCs] = nextSolvable(cs)
	if (nextC.kind === 'EqConst') {
		const su1 = unifies(nextC.ty1, nextC.ty2)
		const su2 = solve(
			new Substitutable(su1).substListCons(Immutable.List(nextCs)).toArray(),
		)
		return compose(su2, su1)
	}
	if (nextC.kind === 'ImpInstConst') {
		return solve([
			{
				kind: 'ExpInstConst',
				ty: nextC.ty1,
				scheme: generalize(nextC.ms, nextC.ty2),
			},
			...nextCs,
		])
	}

	return solve([
		{
			kind: 'EqConst',
			ty1: nextC.ty,
			ty2: instantiate(nextC.scheme),
		},
		...nextCs,
	])
}

export function exec(input: string): [[string, ty.Type][], Error[]] {
	const p = parse(input)
	const [tyContext, errors] = inferTop(TypeEnv.empty(), p)
	return [
		tyContext.toList().map(([name, s]) => [name, s.ty] as [string, ty.Type]),
		errors,
	]
}

export function printTypeWithName(name: string, t: ty.Type): string {
	if (name.startsWith(EXPR_NAME)) {
		name.replace(EXPR_NAME, 'it')
	}
	return `${name} : ${printType(t)}`
}

export function printType(t: ty.Type): string {
	if (t.type === 'Int') {
		return 'Int'
	}
	if (t.type === 'Float') {
		return 'Float'
	}
	if (t.type === 'Bool') {
		return 'Bool'
	}
	if (t.type === 'TVar') {
		return `tVar(${t.id})`
	}
	if (t.type === 'TArr') {
		return `${printType(t.ty1)} -> ${printType(t.ty2)}`
	}
	unreachable()
}
