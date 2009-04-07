package org.planet_sl.apimigration.benchmark.anno;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface Issue {

	public @interface Pre {
		String [] value();
		boolean resolved() default false;
	}
	
	public @interface Post {
		String [] value();
		boolean resolved() default false;
	}
	
	public @interface Invariant {
		String [] value();
		boolean resolved() default false;
	}
	
	public @interface Throws {
		String [] value();
		boolean resolved() default false;
	}
	
	public @interface Doc {
		String [] value();
		boolean resolved() default false;
	}
}
