import fl.types.*;
import tom.library.sl.*;

public class Optimizer{

	%include { sl.tom }
	%include { fl/FL.tom }

	%strategy Step() extends Fail() {
	  visit Expr {
		binary(plus(),x,literal(0)) -> x
		binary(plus(),literal(0),x) -> x
		binary(plus(),literal(i),literal(j)) -> {
			return `literal(i + j); 
		}
		binary(minus(),x,literal(0)) -> x
		binary(minus(),literal(i),literal(j)) -> {
			return `literal(i - j); 
		}
		binary(equal(),x,x) -> literal(-1)
		binary(equal(),literal(i),literal(j)) && i != j ->
			literal(0)
		ifThenElse(literal(0),x,y) -> y 
		ifThenElse(literal(i),x,y) && i != 0 -> x 
		ifThenElse(x,y,y) -> y
		ifThenElse(binary(equal(),x,literal(0)),y,z) ->
			ifThenElse(x,z,y)
		ifThenElse(binary(equal(),literal(0),x),y,z) ->
			ifThenElse(x,z,y)
	   }
	}

	public static Expr optimize(Expr e) {
	   try {
		return (Expr)`Innermost(Step()).visit(e);
	   } catch (VisitFailure x) {
		throw new RuntimeException("Reached dead code.");
	   }
	}
}