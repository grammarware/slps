package org.planet_sl.apimigration.benchmark.jaxb;

import java.io.File;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.planet_sl.apimigration.benchmark.jaxb.company.*;
import org.planet_sl.apimigration.benchmark.scenarios.accessing.Resources;


public class AccessingSalariesUsingJAXB {
	private Company company;
	
	public AccessingSalariesUsingJAXB(String filename) throws JAXBException {
		JAXBContext context = JAXBContext.newInstance(Resources.JAXB_PACKAGE + ".company");
		Unmarshaller unmarshal = context.createUnmarshaller();
		company = (Company) unmarshal.unmarshal(new File(filename));
	}

	
	public void raise(double amount) {
		for (Dept dept: company.getDept()) {
			raiseDepartmentSalaries(dept, amount);
		}
	}
	
	public double sum() {
		double total = 0;
		for (Dept dept: company.getDept()) {
			total += departmentSalaries(dept);
		}
		return total;
	}

	/* 
	 * Helpers
	 */
	
	private double personSalary(Employee emp) {
		if (emp != null) {
			return emp.getSalary();
		}
		return 0;
	}
	
	private void raisePersonSalary(Employee emp, double amount) {
		if (emp != null) {
			emp.setSalary(emp.getSalary() + amount);
		}
	}
	
	private double subUnitSalaries(Subunit sub) {
		double total = 0;
		total += personSalary(sub.getPu());
		Dept nestedDept = sub.getDu();
		if (nestedDept != null) {
			total += departmentSalaries(nestedDept);
		}
		return total;
	}
	
	private void raiseSubUnitSalaries(Subunit sub, double amount) {
		raisePersonSalary(sub.getPu(), amount);
		Dept nestedDept = sub.getDu();
		if (nestedDept != null) {
			raiseDepartmentSalaries(nestedDept, amount);
		}
	}
	
	
	private void raiseDepartmentSalaries(Dept dept, double amount) {
		raisePersonSalary(dept.getManager(), amount);
		for (Subunit sub: dept.getSubunit()) {
			raiseSubUnitSalaries(sub, amount);
		}
	}

	private double departmentSalaries(Dept dept) {
		double total = 0;
		total += personSalary(dept.getManager());
		for (Subunit sub: dept.getSubunit()) {
			total += subUnitSalaries(sub);
		}	
		return total;
	}

	

}
