package org.planet_sl.apimigration.benchmark.scenarios.accessing;

public abstract class AbstractIncreaseSalaries {
	public static final String COMPANY_NS = "http://www.company.com";
	
	public abstract double sum();

	public abstract void increaseSalaries(double amount);

}
