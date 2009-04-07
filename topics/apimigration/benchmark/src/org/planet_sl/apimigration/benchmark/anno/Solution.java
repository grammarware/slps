package org.planet_sl.apimigration.benchmark.anno;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface Solution {
	public enum Strategy {
		MACRO,
		EXTERNAL_MACRO,
		DELEGATE,
		ADVANCED_DELEGATE,
		CLONE,
		OTHER
	}
	Strategy value();
	String comment() default "";
}

