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
	}
	
	public @interface Post {
		String [] value();
	}
	
	public @interface Invariant {
		String [] value();
	}
	
	public @interface Throws {
		String [] value();
	}
	
}
