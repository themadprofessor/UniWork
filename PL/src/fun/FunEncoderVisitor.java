//////////////////////////////////////////////////////////////
//
// A visitor for code generation for Fun.
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
import java.util.List;

import ast.*;

public class FunEncoderVisitor extends AbstractParseTreeVisitor<Void> implements FunVisitor<Void> {

	private SVM obj = new SVM();

	private int globalvaraddr = 0;
	private int localvaraddr = 0;
	private int currentLocale = Address.GLOBAL;

	private SymbolTable<Address> addrTable =
			new SymbolTable<Address>();

	private void predefine () {
		// Add predefined procedures to the address table.
		addrTable.put("read",
				new Address(SVM.READOFFSET, Address.CODE));
		addrTable.put("write",
				new Address(SVM.WRITEOFFSET, Address.CODE));
	}

	public SVM getSVM() {
		return obj;
	}

	/**
	 * Visit a parse tree produced by the {@code prog}
	 * labeled alternative in {@link FunParser#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitProg(FunParser.ProgContext ctx) {
		predefine();
		List<FunParser.Var_declContext> var_decl = ctx.var_decl();
		for (FunParser.Var_declContext vd : var_decl)
			visit(vd);
		int calladdr = obj.currentOffset();
		obj.emit12(SVM.CALL, 0);
		obj.emit1(SVM.HALT);
		List<FunParser.Proc_declContext> proc_decl = ctx.proc_decl();
		for (FunParser.Proc_declContext pd : proc_decl)
			visit(pd);
		int mainaddr = addrTable.get("main").offset;
		obj.patch12(calladdr, mainaddr);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code proc}
	 * labeled alternative in {@link FunParser#proc_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitProc(FunParser.ProcContext ctx) {
		String id = ctx.ID().getText();
		Address procaddr = new Address(obj.currentOffset(), Address.CODE);
		addrTable.put(id, procaddr);
		addrTable.enterLocalScope();
		currentLocale = Address.LOCAL;
		localvaraddr = 2;
		// ... allows 2 words for link data
		FunParser.Formal_declContext fd = ctx.formal_decl();
		if (fd != null)
			visit(fd);
		List<FunParser.Var_declContext> var_decl = ctx.var_decl();
		for (FunParser.Var_declContext vd : var_decl)
			visit(vd);
		visit(ctx.seq_com());
		obj.emit11(SVM.RETURN, 0);
		addrTable.exitLocalScope();
		currentLocale = Address.GLOBAL;
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code func}
	 * labeled alternative in {@link FunParser#proc_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitFunc(FunParser.FuncContext ctx) {
		String id = ctx.ID().getText();
		Address procaddr = new Address(obj.currentOffset(), Address.CODE);
		addrTable.put(id, procaddr);
		addrTable.enterLocalScope();
		currentLocale = Address.LOCAL;
		localvaraddr = 2;
		// ... allows 2 words for link data
		FunParser.Formal_declContext fd = ctx.formal_decl();
		if (fd != null)
			visit(fd);
		List<FunParser.Var_declContext> var_decl = ctx.var_decl();
		for (FunParser.Var_declContext vd : var_decl)
			visit(vd);
		visit(ctx.seq_com());
		visit(ctx.expr());
		obj.emit11(SVM.RETURN, 1);
		addrTable.exitLocalScope();
		currentLocale = Address.GLOBAL;
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code formal}
	 * labeled alternative in {@link FunParser#formal_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitFormal(FunParser.FormalContext ctx) {
		FunParser.TypeContext tc = ctx.type();
		if (tc != null) {
			String id = ctx.ID().getText();
			addrTable.put(id, new Address(localvaraddr++, Address.LOCAL));
			obj.emit11(SVM.COPYARG, 1);
		}
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code var}
	 * labeled alternative in {@link FunParser#var_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitVar(FunParser.VarContext ctx) {
		visit(ctx.expr());
		String id = ctx.ID().getText();
		switch (currentLocale) {
			case Address.LOCAL:
				addrTable.put(id, new Address(
						localvaraddr++, Address.LOCAL));
				break;
			case Address.GLOBAL:
				addrTable.put(id, new Address(
						globalvaraddr++, Address.GLOBAL));
		}
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code bool}
	 * labeled alternative in {@link FunParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitBool(FunParser.BoolContext ctx) {
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code int}
	 * labeled alternative in {@link FunParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitInt(FunParser.IntContext ctx) {
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code assn}
	 * labeled alternative in {@link FunParser#com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitAssn(FunParser.AssnContext ctx) {
		visit(ctx.expr());
		String id = ctx.ID().getText();
		Address varaddr = addrTable.get(id);
		switch (varaddr.locale) {
			case Address.GLOBAL:
				obj.emit12(SVM.STOREG,varaddr.offset);
				break;
			case Address.LOCAL:
				obj.emit12(SVM.STOREL,varaddr.offset);
		}
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code proccall}
	 * labeled alternative in {@link FunParser#com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitProccall(FunParser.ProccallContext ctx) {
		visit(ctx.actual());
		String id = ctx.ID().getText();
		Address procaddr = addrTable.get(id);
		// Assume procaddr.locale == CODE.
		obj.emit12(SVM.CALL,procaddr.offset);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code if}
	 * labeled alternative in {@link FunParser#com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitIf(FunParser.IfContext ctx) {
		visit(ctx.expr());
		int condaddr = obj.currentOffset();
		obj.emit12(SVM.JUMPF, 0);
		if (ctx.c2 == null) { // IF without ELSE
			visit(ctx.c1);
			int exitaddr = obj.currentOffset();
			obj.patch12(condaddr, exitaddr);
		}
		else {                // IF ... ELSE
			visit(ctx.c1);
			int jumpaddr = obj.currentOffset();
			obj.emit12(SVM.JUMP, 0);
			int elseaddr = obj.currentOffset();
			obj.patch12(condaddr, elseaddr);
			visit(ctx.c2);
			int exitaddr = obj.currentOffset();
			obj.patch12(jumpaddr, exitaddr);
		}
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code while}
	 * labeled alternative in {@link FunParser#com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitWhile(FunParser.WhileContext ctx) {
		int startaddr = obj.currentOffset();
		visit(ctx.expr());
		int condaddr = obj.currentOffset();
		obj.emit12(SVM.JUMPF, 0);
		visit(ctx.seq_com());
		obj.emit12(SVM.JUMP, startaddr);
		int exitaddr = obj.currentOffset();
		obj.patch12(condaddr, exitaddr);
		return null;
	}

	// EXTENSION
	/*
	fn for {
		load initialValue
		assign initialValue to iterVar
		load iterVar
		loop {
			load limitVar
			if iterVar < limitVar {
				visitBody()
				load iterVar
				increment
			} else {
				break
			}
		}
	}
	 */
	@Override
	public Void visitFor(FunParser.ForContext ctx) {
		Address indexLoc = addrTable.get(ctx.ID().getText());
		byte load = indexLoc.locale == Address.GLOBAL ? SVM.LOADG : SVM.LOADL;
		byte store = indexLoc.locale == Address.GLOBAL ? SVM.STOREG : SVM.STOREL;

		visit(ctx.expr(0)); // Visit assignment expression
        obj.emit12(store, indexLoc.offset); // Update memory
        int startAddr = obj.currentOffset(); // Remember start of loop
        obj.emit12(load, indexLoc.offset);

        visit(ctx.expr(1)); // Visit limit expression

        obj.emit1(SVM.CMPLT);
        int condAddr = obj.currentOffset(); // Remember condition location
        obj.emit12(SVM.JUMPF, 0);

        visit(ctx.seq_com()); // Visit body of for loop

		obj.emit12(load, indexLoc.offset); // Increment index
		obj.emit1(SVM.INC);
		obj.emit12(store, indexLoc.offset);
		obj.emit12(SVM.JUMP, startAddr); // Go to top of loop
		obj.patch12(condAddr, obj.currentOffset()); // Update condition jump

		return null;
	}

	// EXTENSION
	/*
	fn switch() {
		for case in cases {
			case.checkAndRun()

			if (ranCase) {
				break
			}
		}
		if (!ranCase) {
			default()
		}
	}
	 */
	@Override
	public Void visitSwitch(FunParser.SwitchContext ctx) {
		ArrayList<Integer> successJumps = new ArrayList<>(ctx.case_stmt().size());
		int prevJump = -1;

		// Generate each case
		List<FunParser.Case_stmtContext> case_stmt = ctx.case_stmt();
		for (int i = 0, case_stmtSize = case_stmt.size(); i < case_stmtSize; i++) {
			visit(ctx.expr());
			FunParser.Case_stmtContext case_stmtContext = case_stmt.get(i);
			if (i != 0) {
				obj.patch12(prevJump, obj.currentOffset());
			}

			visit(case_stmtContext);

			// If actually ran block, jump to end of switch
			obj.emit12(SVM.LOADC, 1);
			obj.emit1(SVM.CMPEQ);
			prevJump = obj.currentOffset();
			obj.emit12(SVM.JUMPF, 0);

			successJumps.add(obj.currentOffset());
			obj.emit12(SVM.JUMP, 0);
		}
		if (prevJump != -1) {
		    // Only update if there is a previous jump
			obj.patch12(prevJump, obj.currentOffset());
		}

		// Generate default case
		visit(ctx.default_stmt());

		// Update the jumps
		for (Integer jump : successJumps) {
			obj.patch12(jump, obj.currentOffset());
		}

		return null;
	}

	// EXTENSION
	/*
	fn default() {
		body()
	}
	 */
	@Override
	public Void visitDefault_stmt(FunParser.Default_stmtContext ctx) {
		visit(ctx.seq_com());

		return null;
	}

	// EXTENSION
	/*
	fn case.checkAndRun() {
		if (isLiteral) {
			load parent.switchVar
			load literal
			if (parent.switchVar == literal) {
				body()
				ranCase = true
			} else {
				ranCase = false
			}
		} else {
			load parent.switchVar
			load lowerBound
			if (parent.switchVar == lowerBound) {
				body()
				ranCase = true
				return
			}

			load parent.switchVar
			load upperBound
			if (parent.switchVar == upperBound) {
				body()
				ranCase = true
				return
			}

			load parent.switchVar
			load lowerBound
			load upperBound

			if (parent.switchVar > lowerBound && parent.switchVar < upperBound) {
				body()
				ranCase = true
				return
			}
			ranCase = false
		}
	}
	 */
	@Override
	public Void visitCase_stmt(FunParser.Case_stmtContext ctx) {
		if (ctx.raw_lit() != null) { // not a range
			visit(ctx.raw_lit());
			//visit(((FunParser.SwitchContext) ctx.parent).expr());

            // If case equals variable, jump to block, otherwise push false and jump over block
			obj.emit1(SVM.CMPEQ);
			int success = obj.currentOffset();
			obj.emit12(SVM.JUMPT, 0);
			obj.emit12(SVM.LOADC, 0);
			int fail = obj.currentOffset();
			obj.emit12(SVM.JUMP, 0);

			// Update equals jump
			obj.patch12(success, obj.currentOffset());
			visit(ctx.seq_com());
			// Push true
			obj.emit12(SVM.LOADC, 1);

			// Update not-equal jump
			obj.patch12(fail, obj.currentOffset());
		} else {
			int lowerEqJump, upperEqJump, betweenJump, failJump;
			int lower = Integer.parseInt(ctx.NUM(0).getText());
			int upper = Integer.parseInt(ctx.NUM(1).getText());

			// If equal to lower bound, jump to body
			obj.emit12(SVM.LOADC, lower);
			//visit(((FunParser.SwitchContext) ctx.parent).expr());
			obj.emit1(SVM.CMPEQ);
			lowerEqJump = obj.currentOffset();
			obj.emit12(SVM.JUMPT, 0);

			// If equal to upper bound, jump to body
			obj.emit12(SVM.LOADC, upper);
			visit(((FunParser.SwitchContext) ctx.parent).expr());
			obj.emit1(SVM.CMPEQ);
			upperEqJump = obj.currentOffset();
			obj.emit12(SVM.JUMPT, 0);

			// Check if greater than lower bound
			obj.emit12(SVM.LOADC, lower);
			visit(((FunParser.SwitchContext) ctx.parent).expr());
			obj.emit1(SVM.CMPGT);

			// Check if less than upper bound
			obj.emit12(SVM.LOADC, upper);
			visit(((FunParser.SwitchContext) ctx.parent).expr());
			obj.emit1(SVM.CMPLT);

			// If both checks are equal, must be within bounds
			// Impossible to be greater than upper and less than lower
			obj.emit1(SVM.CMPEQ);
			betweenJump = obj.currentOffset();
			obj.emit12(SVM.JUMPT, 0);

			// Failed to match any, push false and jump over block
			obj.emit12(SVM.LOADC, 0);
			failJump = obj.currentOffset();
			obj.emit12(SVM.JUMP, 0);

			// Update equality jumps
			obj.patch12(lowerEqJump, obj.currentOffset());
			obj.patch12(upperEqJump, obj.currentOffset());
			obj.patch12(betweenJump, obj.currentOffset());

			visit(ctx.seq_com());
			obj.emit12(SVM.LOADC, 1);

			obj.patch12(failJump, obj.currentOffset());
		}

		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code seq}
	 * labeled alternative in {@link FunParser#seq_com}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitSeq(FunParser.SeqContext ctx) {
		visitChildren(ctx);
		return null;
	}

	/**
	 * Visit a parse tree produced by {@link FunParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitExpr(FunParser.ExprContext ctx) {
		visit(ctx.e1);
		if (ctx.e2 != null) {
			visit(ctx.e2);
			switch (ctx.op.getType()) {
				case FunParser.EQ:
					obj.emit1(SVM.CMPEQ);
					break;
				case FunParser.LT:
					obj.emit1(SVM.CMPLT);
					break;
				case FunParser.GT:
					obj.emit1(SVM.CMPGT);
					break;
			}
		}
		return null;
	}

	/**
	 * Visit a parse tree produced by {@link FunParser#sec_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitSec_expr(FunParser.Sec_exprContext ctx) {
		visit(ctx.e1);
		if (ctx.e2 != null) {
			visit(ctx.e2);
			switch (ctx.op.getType()) {
				case FunParser.PLUS:
					obj.emit1(SVM.ADD);
					break;
				case FunParser.MINUS:
					obj.emit1(SVM.SUB);
					break;
				case FunParser.TIMES:
					obj.emit1(SVM.MUL);
					break;
				case FunParser.DIV:
					obj.emit1(SVM.DIV);
					break;
			}
		}
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code false}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitFalse(FunParser.FalseContext ctx) {
		obj.emit12(SVM.LOADC, 0);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code true}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitTrue(FunParser.TrueContext ctx) {
		obj.emit12(SVM.LOADC, 1);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code num}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitNum(FunParser.NumContext ctx) {
		int value = Integer.parseInt(ctx.NUM().getText());
		obj.emit12(SVM.LOADC, value);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code id}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitId(FunParser.IdContext ctx) {
		String id = ctx.ID().getText();
		Address varaddr = addrTable.get(id);
		switch (varaddr.locale) {
			case Address.GLOBAL:
				obj.emit12(SVM.LOADG,varaddr.offset);
				break;
			case Address.LOCAL:
				obj.emit12(SVM.LOADL,varaddr.offset);
		}
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code funccall}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitFunccall(FunParser.FunccallContext ctx) {
		visit(ctx.actual());
		String id = ctx.ID().getText();
		Address funcaddr = addrTable.get(id);
		// Assume that funcaddr.locale == CODE.
		obj.emit12(SVM.CALL,funcaddr.offset);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code not}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitNot(FunParser.NotContext ctx) {
		visit(ctx.prim_expr());
		obj.emit1(SVM.INV);
		return null;
	}

	/**
	 * Visit a parse tree produced by the {@code parens}
	 * labeled alternative in {@link FunParser#prim_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitParens(FunParser.ParensContext ctx) {
		visit(ctx.expr());
		return null;
	}

	/**
	 * Visit a parse tree produced by {@link FunParser#actual}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	public Void visitActual(FunParser.ActualContext ctx) {
		FunParser.ExprContext ec = ctx.expr();
		if (ec != null) {
			visit(ec);
		}
		return null;
	}

	@Override
	public Void visitRaw_false(FunParser.Raw_falseContext ctx) {
		obj.emit12(SVM.LOADC, 0);
		return null;
	}

	@Override
	public Void visitRaw_true(FunParser.Raw_trueContext ctx) {
		obj.emit12(SVM.LOADC, 1);
		return null;
	}

	@Override
	public Void visitRaw_num(FunParser.Raw_numContext ctx) {
		obj.emit12(SVM.LOADC, Integer.parseInt(ctx.NUM().getText()));
		return null;
	}
}
