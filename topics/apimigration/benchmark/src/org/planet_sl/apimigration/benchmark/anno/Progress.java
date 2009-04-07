package org.planet_sl.apimigration.benchmark.anno;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface Progress {
	public enum Status {
		TODO,
		NEEDSWORK,
		OK,
		DONTCARE,
		GIVENUP
	}
	Status value() default Status.TODO;
	String comment() default "";
}
