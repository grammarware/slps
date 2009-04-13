package org.planet_sl.apimigration.benchmark.anno;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface Unresolved {
	public enum XML {
		BaseURI,
		Escaping,
		Namespacing,
		Serialization,
		Parsing, DocTypeValidity;		
	}
	
	XML[] value();
}
