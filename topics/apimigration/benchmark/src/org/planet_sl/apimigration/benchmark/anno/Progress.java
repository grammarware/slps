package org.planet_sl.apimigration.benchmark.anno;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface Progress {
	public enum Status {
		// TODO: compliance level 4!!!
		TODO() {
			public String toString() {
				return "1";
			}
		},
		NEEDSWORK() {
			public String toString() {
				return "2";
			}
		},
		OK() {
			public String toString() {
				return "3";
			}
		},
		DONTCARE() {
			public String toString() {
				return "1";
			}
		},
		GIVENUP() {
			public String toString() {
				return "1";
			}
		};
	}
	Status value() default Status.TODO;
	String comment() default "";
}
