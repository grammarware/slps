package org.planet_sl.apimigration.benchmark.anno;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface Solution {
	public enum Strategy {
		MACRO() {
			public String toString() {
				return "3";
			}
		},
		DELEGATE() {
			public String toString() {
				return "1";
			}
		},
		ADVANCED_DELEGATE() {
			public String toString() {
				return "2";
			}
		},
		CLONE() {
			public String toString() {
				return "4";
			}
		},
		OTHER() {
			public String toString() {
				return "O";
			}
		}
	}
	Strategy value();
	String comment() default "";
}

