//////////////////////////////////////////////////////////////
//
// A visitor for contextual analysis of Fun.
//
// Developed August 2015 by Simon Gay (University of Glasgow).
//
// Based on a previous version by David Watt.
//
// Add for loop and switch statement support by Stuart Reilly February 2020
//
//////////////////////////////////////////////////////////////

package fun;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import ast.*;

public class FunCheckerVisitor extends AbstractParseTreeVisitor<Type> implements FunVisitor<Type> {

	// Contextual errors

	private int errorCount = 0;

	private CommonTokenStream tokens;

	// Constructor

	public FunCheckerVisitor(CommonTokenStream toks) {
		tokens = toks;
	}

	private void reportError (String message,
							  ParserRuleContext ctx) {
		// Print an error message relating to the given
		// part of the AST.
		Interval interval = ctx.getSourceInterval();
		Token start = tokens.get(interval.a);
		Token finish = tokens.get(interval.b);
		int startLine = start.getLine();
		int startCol = start.getCharPositionInLine();
		int finishLine = finish.getLine();
		int finishCol = finish.getCharPositionInLine();
		System.err.println(startLine + ":" + startCol + "-" +
				finishLine + ":" + finishCol
				+ " " + message);
		errorCount++;
	}

	public int getNumberOfContextualErrors () {
		// Return the total number of errors so far detected.
		return errorCount;
	}


	// Scope checking

	private SymbolTable<Type> typeTable =
			new SymbolTable<Type>();

	private void predefine () {
		// Add predefined procedures to the type table.
		typeTable.put("read",
				new Type.Mapping(Type.VOID, Type.INT));
		typeTable.put("write",
				new Type.Mapping(Type.INT, Type.VOID));
	}

	private void define (String id, Type type,
						 ParserRuleContext decl) {
		// Add id with its type to the type table, checking
		// that id is not already declared in the same scope.
		boolean ok = typeTable.put(id, type);
		if (!ok)
			reportError(id + " is redeclared", decl);
	}

	private Type retrieve (String id, ParserRuleContext occ) {
		// Retrieve id's type from the type table.
		Type type = typeTable.get(id);
		if (type == null) {
			reportError(id + " is undeclared", occ);
			return Type.ERROR;
		} else
			return type;
	}

	// Type checking

	private static final Type.Mapping
			NOTTYPE = new Type.Mapping(Type.BOOL, Type.BOOL),
			COMPTYPE = new Type.Mapping(
					new Type.Pair(Type.INT, Type.INT), Type.BOOL),
			ARITHTYPE = new Type.Mapping(
					new Type.Pair(Type.INT, Type.INT), Type.INT),
			MAINTYPE = new Type.Mapping(Type.VOID, Type.VOID);

	private void checkType (Type typeExpected,
							Type typeActual,
							ParserRuleContext construct) {
		// Check that a construct's actual type matches
		// the expected type.
		if (! typeActual.equiv(typeExpected))
			reportError("type is " + typeActual
							+ ", should be " + typeExpected,
					construct);
	}

	// EXTENSION
	private void checkType(Type[] expected, Type actual, ParserRuleContext construct) {
		boolean valid = false;

		for (Type type : expected) {
			if (actual.equiv(type)) {
			    valid = true;
			    break;
			}
		}

		if (!valid) {
			reportError("type is " + actual + ", should be one of " + Arrays.toString(expected), construct);
		}
	}

	private Type checkCall (String id, Type typeArg,
							ParserRuleContext call) {
		// Check that a procedure call identifies a procedure
		// and that its argument type matches the proecure's
		// type. Return the type of the procedure call.
		Type typeProc = retrieve(id, call);
		if (! (typeProc instanceof Type.Mapping)) {
			reportError(id + " is not a procedure", call);
			return Type.ERROR;
		} else {
			Type.Mapping mapping = (Type.Mapping)typeProc;
			checkType(mapping.domain, typeArg, call);
			return mapping.range;
		}
	}

	private Type checkUnary (Type.Mapping typeOp,
							 Type typeArg,
							 ParserRuleContext op) {
		// Check that a unary operator's operand type matches
		// the operator's type. Return the type of the operator
		// application.
		if (! (typeOp.domain instanceof Type.Primitive))
			reportError(
					"unary operator should have 1 operand",
					op);
		else
			checkType(typeOp.domain, typeArg, op);
		return typeOp.range;
	}

	private Type checkBinary (Type.Mapping typeOp,
							  Type typeArg1, Type typeArg2,
							  ParserRuleContext op) {
		// Check that a binary operator's operand types match
		// the operator's type. Return the type of the operator
		// application.
		if (! (typeOp.domain instanceof Type.Pair))
			reportError(
					"binary operator should have 2 operands",
					op);
		else {
			Type.Pair pair =
					(Type.Pair)typeOp.domain;
			checkType(pair.first, typeArg1, op);
			checkType(pair.second, typeArg2, op);
		}
		return typeOp.range;
	}

	/**
	 * Visit a parse tree produced by the {@code prog}
	 * labeled alternative in {@link FunParser#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitProg(FunParser.ProgContext ctx) {
		predefine();
		visitChildren(ctx);
		Type tmain = retrieve("main", ctx);
		checkType(MAINTYPE, tmain, ctx);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code proc}
	 * labeled alternative in {@link FunParser#proc_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitProc(FunParser.ProcContext ctx) {
		typeTable.enterLocalScope();
		Type t;
		FunParser.Formal_declContext fd = ctx.formal_decl();
		if (fd != null)
			t = visit(fd);
		else
			t = Type.VOID;
		Type proctype = new Type.Mapping(t, Type.VOID);
		define(ctx.ID().getText(), proctype, ctx);
		List<FunParser.Var_declContext> var_decl = ctx.var_decl();
		for (FunParser.Var_declContext vd : var_decl)
			visit(vd);
		visit(ctx.seq_com());
		typeTable.exitLocalScope();
		define(ctx.ID().getText(), proctype, ctx);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code func}
	 * labeled alternative in {@link FunParser#proc_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitFunc(FunParser.FuncContext ctx) {
		typeTable.enterLocalScope();
		Type t1 = visit(ctx.type());
		Type t2;
		FunParser.Formal_declContext fd = ctx.formal_decl();
		if (fd != null)
			t2 = visit(fd);
		else
			t2 = Type.VOID;
		Type functype = new Type.Mapping(t2, t1);
		define(ctx.ID().getText(), functype, ctx);
		List<FunParser.Var_declContext> var_decl = ctx.var_decl();
		for (FunParser.Var_declContext vd : var_decl)
			visit(vd);
		visit(ctx.seq_com());
		Type returntype = visit(ctx.expr());
		checkType(t1, returntype, ctx);
		typeTable.exitLocalScope();
		define(ctx.ID().getText(), functype, ctx);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code formal}
	 * labeled alternative in {@link FunParser#formal_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitFormal(FunParser.FormalContext ctx) {
		FunParser.TypeContext tc = ctx.type();
		Type t;
		if (tc != null) {
			t = visit(tc);
			define(ctx.ID().getText(), t, ctx);
		}
		else
			t = Type.VOID;
		return t;
	}

	/**
	 * Visit a parse tree produced by the {@code var}
	 * labeled alternative in {@link FunParser#var_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitVar(FunParser.VarContext ctx) {
		Type t1 = visit(ctx.type());
		Type t2 = visit(ctx.expr());
		define(ctx.ID().getText(), t1, ctx);
		checkType(t1, t2, ctx);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code bool}
	 * labeled alternative in {@link FunParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitBool(FunParser.BoolContext ctx) {
		return Type.BOOL;
	}

	/**
	 * Visit a parse tree produced by the {@code int}
	 * labeled alternative in {@link FunParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitInt(FunParser.IntContext ctx) {
		return Type.INT;
	}

	/**
	 * Visit a parse tree produced by the {@code assn}
	 * labeled alternative in {@link FunParser#com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitAssn(FunParser.AssnContext ctx) {
		Type tvar = retrieve(ctx.ID().getText(), ctx);
		Type t = visit(ctx.expr());
		checkType(tvar, t, ctx);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code proccall}
	 * labeled alternative in {@link FunParser#com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitProccall(FunParser.ProccallContext ctx) {
		Type t = visit(ctx.actual());
		Type tres = checkCall(ctx.ID().getText(), t, ctx);
		if (! tres.equiv(Type.VOID))
			reportError("procedure should be void", ctx);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code if}
	 * labeled alternative in {@link FunParser#com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitIf(FunParser.IfContext ctx) {
		Type t = visit(ctx.expr());
		visit(ctx.c1);
		if (ctx.c2 != null)
			visit(ctx.c2);
		checkType(Type.BOOL, t, ctx);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code while}
	 * labeled alternative in {@link FunParser#com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitWhile(FunParser.WhileContext ctx) {
		Type t = visit(ctx.expr());
		visit(ctx.seq_com());
		checkType(Type.BOOL, t, ctx);
		return null;
	}

	// EXTENSION
	@Override
	public Type visitFor(FunParser.ForContext ctx) {
	    Type assignExpr = visit(ctx.expr(0));
	    Type limitExpr = visit(ctx.expr(1));
	    Type varType = typeTable.get(ctx.ID().getText());

		visit(ctx.seq_com());
		checkType(Type.INT, assignExpr, ctx);
	    checkType(Type.INT, limitExpr, ctx);
	    if (varType == null) {
	    	reportError(ctx.ID().getText() + " is not defined", ctx);
		} else {
			checkType(Type.INT, varType, ctx);
		}

		return null;
	}

	// EXTENSION
	@Override
	public Type visitSwitch(FunParser.SwitchContext ctx) {
		Type expr = visit(ctx.expr());
		List<Tuple<int[], FunParser.Case_stmtContext>> case_vals = new ArrayList<>(ctx.case_stmt().size());

		for (FunParser.Case_stmtContext case_stmtContext : ctx.case_stmt()) {
			Type caseType = visit(case_stmtContext);
			if (caseType instanceof Type.Pair) {
				checkType(expr, ((Type.Pair) caseType).first, case_stmtContext);
				int lower = Integer.parseInt(case_stmtContext.NUM(0).getText());
				int upper = Integer.parseInt(case_stmtContext.NUM(1).getText());

				if (upper < lower) {
					reportError("left value must be lower than the right in a range", case_stmtContext);
				}

				case_vals.add(new Tuple<>(new int[]{lower, upper}, case_stmtContext));
			} else {
				checkType(expr, visit(case_stmtContext), case_stmtContext);
				int[] vals;

				if (caseType == Type.INT) {
					vals = new int[] {Integer.parseInt(case_stmtContext.raw_lit().getText())};
				} else if (caseType == Type.BOOL) {
					vals = new int[] {Boolean.parseBoolean(case_stmtContext.raw_lit().getText()) ? 0 : 1};
				} else {
					vals = new int[] {}; // Invalid type
				}

				case_vals.add(new Tuple<>(vals, case_stmtContext));
			}
		}

		checkType(new Type[]{Type.INT, Type.BOOL}, expr, ctx); // Only allow int or bool expression

		for (int i = 0; i < case_vals.size(); i++) {
			for (int j = i+1; j < case_vals.size(); j++) {
				Tuple<int[], FunParser.Case_stmtContext> left = case_vals.get(i);
				Tuple<int[], FunParser.Case_stmtContext> right = case_vals.get(j);

				if (left.a.length == 1) {
					if (right.a.length == 1) {
						// Both literals
						if (left.a[0] == right.a[0]) {
							reportError(Util.overlapErrorString(left.a, right.a), left.b);
						}
					} else if (right.a.length == 2) {
						// Left is literal, right is pair
						if (right.a[0] < left.a[0] && right.a[1] > left.a[0]) {
							reportError(Util.overlapErrorString(left.a, right.a), left.b);
						}
					} else {
						// Right is void
						reportError("case " + right.b.getText() + " is VOID", right.b);
					}
				} else if (left.a.length == 2) {
					if (right.a.length == 1) {
						// Left is pair, right is literal
						if (left.a[0] < right.a[0] && left.a[1] > right.a[0]) {
							reportError(Util.overlapErrorString(left.a, right.a), right.b);
						}
					} else if (right.a.length == 2) {
						// Both pairs
						if (left.a[0] <= right.a[1] && right.a[0] <= left.a[1]) {
							reportError(Util.overlapErrorString(left.a, right.a), left.b);
						}
					} else {
						// Right is void
						reportError("case " + right.b.getText() + " is VOID", right.b);
					}
				} else {
					reportError("case " + left.b.getText() + " is VOID", left.b);
				}
			}
		}

		return null;
	}

	// EXTENSION
	@Override
	public Type visitDefault_stmt(FunParser.Default_stmtContext ctx) {
		visit(ctx.seq_com());
		return null;
	}

	// EXTENSION
	@Override
	public Type visitCase_stmt(FunParser.Case_stmtContext ctx) {
	    Type t;
		if (ctx.raw_lit() != null) {
			// Not a range
			t = visit(ctx.raw_lit());
		} else {
			t = new Type.Pair(Type.INT, Type.INT);
		}
		visit(ctx.seq_com());

		return t;
	}

	/**
	 * Visit a parse tree produced by the {@code seq}
	 * labeled alternative in {@link FunParser#seq_com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitSeq(FunParser.SeqContext ctx) {
		visitChildren(ctx);
		return null;
	}

	/**
	 * Visit a parse tree produced by {@link FunParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitExpr(FunParser.ExprContext ctx) {
		Type t1 = visit(ctx.e1);
		if (ctx.e2 != null) {
			Type t2 = visit(ctx.e2);
			return checkBinary(COMPTYPE, t1, t2, ctx);
		}
		else {
			return t1;
		}
	}

	/**
	 * Visit a parse tree produced by {@link FunParser#sec_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitSec_expr(FunParser.Sec_exprContext ctx) {
		Type t1 = visit(ctx.e1);
		if (ctx.e2 != null) {
			Type t2 = visit(ctx.e2);
			return checkBinary(ARITHTYPE, t1, t2, ctx);
		}
		else {
			return t1;
		}
	}

	/**
	 * Visit a parse tree produced by the {@code false}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitFalse(FunParser.FalseContext ctx) {
		return Type.BOOL;
	}

	/**
	 * Visit a parse tree produced by the {@code true}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitTrue(FunParser.TrueContext ctx) {
		return Type.BOOL;
	}

	/**
	 * Visit a parse tree produced by the {@code num}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitNum(FunParser.NumContext ctx) {
		return Type.INT;
	}

	/**
	 * Visit a parse tree produced by the {@code id}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitId(FunParser.IdContext ctx) {
		return retrieve(ctx.ID().getText(), ctx);
	}

	/**
	 * Visit a parse tree produced by the {@code funccall}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitFunccall(FunParser.FunccallContext ctx) {
		Type t = visit(ctx.actual());
		Type tres = checkCall(ctx.ID().getText(), t, ctx);
		if (tres.equiv(Type.VOID))
			reportError("procedure should be non-void", ctx);
		return tres;
	}

	/**
	 * Visit a parse tree produced by the {@code not}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitNot(FunParser.NotContext ctx) {
		Type t = visit(ctx.prim_expr());
		return checkUnary(NOTTYPE, t, ctx);
	}

	/**
	 * Visit a parse tree produced by the {@code parens}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitParens(FunParser.ParensContext ctx) {
		return visit(ctx.expr());
	}

	/**
	 * Visit a parse tree produced by {@link FunParser#actual}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Type visitActual(FunParser.ActualContext ctx) {
		FunParser.ExprContext ec = ctx.expr();
		Type t;
		if (ec != null) {
			t = visit(ec);
		}
		else
			t = Type.VOID;
		return t;
	}

	// EXTENSION
	@Override
	public Type visitRaw_false(FunParser.Raw_falseContext ctx) {
		return Type.BOOL;
	}

	// EXTENSION
	@Override
	public Type visitRaw_true(FunParser.Raw_trueContext ctx) {
		return Type.BOOL;
	}

	// EXTENSION
	@Override
	public Type visitRaw_num(FunParser.Raw_numContext ctx) {
		return Type.INT;
	}
}
