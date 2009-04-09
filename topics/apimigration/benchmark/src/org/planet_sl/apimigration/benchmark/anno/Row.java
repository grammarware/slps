package org.planet_sl.apimigration.benchmark.anno;

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

public class Row {

	AccessibleObject from;
	MapsTo to;
	
	Progress progress;
	Solution solution;
	
	Issue.Throws throwsIssue;
	Issue.Pre preIssue;
	Issue.Post postIssue;
	Issue.Invariant invariantIssue;
	Issue.Doc docIssue;
	
	
	public static String HEADER = q("xom.nu") + ", " + q("org.jdom") + ", " + q("Status") + ", " + q("Strategy")
		+ ", " + q("Pre") + ", " + q("Post") + ", " + q("Throws") + ", " + q("Invariant") + ", " + q("Doc");
	
	public String toString() {
		return q(fromString()) + ", " + q(mapsToString()) + ", " + q(progressString()) + ", " + q(solutionString()) + ", "
			+ issuesString(preIssues()) + ", " + issuesString(postIssues()) + ", "+ issuesString(throwsIssues()) + ", " 
			+ issuesString(invariantIssues()) + ", " + issuesString(docIssues()); 
	}

	
	
	
	private String progressString() {
		if (progress == null) {
			return "";
		}
		return progress.value().toString();
	}




	private String solutionString() {
		if (solution == null) {
			return "";
		}
		return solution.value().toString();
	}




	private String[] docIssues() {
		if (docIssue != null) {
			return docIssue.value();
		}
		String result[] = {};
		return result;
	}




	private String[] invariantIssues() {
		if (invariantIssue != null) {
			return invariantIssue.value();
		}
		String result[] = {};
		return result;
	}




	private String[] throwsIssues() {
		if (throwsIssue != null) {
			return throwsIssue.value();
		}
		String result[] = {};
		return result;
	}




	private String[] postIssues() {
		if (postIssue != null) {
			return postIssue.value();
		}
		String result[] = {};
		return result;
	}




	private String[] preIssues() {
		if (preIssue != null) {
			return preIssue.value();
		}
		String result[] = {};
		return result;
	}




	private String issuesString(String issues[]) {
		if (issues.length == 0) {
			return q("");
		}
		String result = "";
		for (int i = 0; i < issues.length; i++) {
			result += issues[i];
			if (i < issues.length - 1) {
				result += "; ";
			}
		}
		return q(result);
	}

	private String mapsToString() {
		if (to != null) {
			return to.value().replaceAll("org\\.jdom\\.", "").replaceAll(",", ", ");
		}
		throw new RuntimeException("missing mapsto annotation: " + from);
	}

	private String methodString(Method method) {
		String result = method.getDeclaringClass().getSimpleName();
		result += "#" + method.getName();
		Class<?> types[] =  method.getParameterTypes();
		result += "(";
		for (int i = 0; i < types.length; i++) {
			result += types[i].getSimpleName();
			if (i < types.length - 1) {
				result += ", ";
			}
		}
		result += ")";
		return result;
	}
	
	private String constructorString(Constructor<?> cons) {
		String result = cons.getDeclaringClass().getSimpleName();
		Class<?> types[] =  cons.getParameterTypes();
		result += "(";
		for (int i = 0; i < types.length; i++) {
			result += types[i].getSimpleName();
			if (i < types.length - 1) {
				result += ", ";
			}
		}
		result += ")";
		return result;
	}
	
	private String fromString() {
		if (from instanceof Method) {
			return methodString((Method)from);
		}
		if (from instanceof Constructor<?>) {
			return constructorString((Constructor<?>) from);
		}
		throw new AssertionError("from is an accessible object that is not Method/Constructor");
	}

	static String q(Object o) {
		if (o == null) {
			o = "";
		}
		return "\"" + o.toString().replaceAll("\"", "\\\"") + "\"";
	}
	
}
