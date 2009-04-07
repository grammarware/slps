package org.planet_sl.apimigration.benchmark.anno;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface API {
	public enum Kind {
		SIMILAR, 
		MACRO,
		CORE,
		NAMING,
		EQUAL,
		IDIOM,
		PERF,
		ARG,
		RESULT,
		THROWS,
		PRE,
		POST,
		HIER,
		TYPE, // type encoding e.g.: classes vs. ints vs. enums etc.
		DATA, // data encoding, e.g.  colons in name-strings.
		INV;

		} 
	
	Kind[] value() default {Kind.SIMILAR};
	String[] doc() default "";
}
