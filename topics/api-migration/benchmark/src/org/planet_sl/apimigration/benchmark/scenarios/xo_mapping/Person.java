package org.planet_sl.apimigration.benchmark.scenarios.xo_mapping;

import java.util.List;
import java.util.Vector;

public class Person {

	public static final Person OBAMA = new Person("Barack Obama", 47);
	public static final Person MCCAIN = new Person("John McCain", 72);
	public static final List<Person> PERSONS;
	
	static {
		PERSONS = new Vector<Person>();
		PERSONS.add(OBAMA);
		PERSONS.add(MCCAIN);
	}
	
	private String name;
	private int age;
	
	public Person(String name, int age) {
		this.name = name;
		this.age = age;
	}
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public int getAge() {
		return age;
	}
	public void setAge(int age) {
		this.age = age;
	}
	
	public String toString() {
		return "{name: " + name + ", " + "age: " + age + "}";
	}
	
	public boolean equals(Object other) {
		if (!(other instanceof Person)) {
			return false;
		}
		Person that = (Person)other;
		return this.name.equals(that.name) && this.age == that.age;
	}
	
}
