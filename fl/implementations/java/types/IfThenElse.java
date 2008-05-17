/**
 * IfThenElse expression form
 */
public class IfThenElse extends Expr {
	public Expr exprIf, exprThen, exprElse;
	public IfThenElse(Expr exprIf, Expr exprThen, Expr exprElse) {
		this.exprIf = exprIf;
		this.exprThen = exprThen;
		this.exprElse = exprElse;
	}
	public void accept(Visitor v) {
		v.visit(this);
	}
}
