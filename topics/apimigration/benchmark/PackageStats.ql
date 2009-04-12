/**
 * @name PackageStats
 */
import default

from Package package, int numberOfTypes
where
 (package.getName().matches("org.jdom%") 
  or
	(
  package.getName().matches("nu.xom%")
  and not package.getName() = "nu.xom.tests"
  and not package.getName() = "nu.xom.benchmarks"
  ) 
 ) and
	numberOfTypes = count(Class c | package.contains(c) and c.hasModifier("public") and not c.isAnonymous())
			+ count(Interface c | package.contains(c) and c.hasModifier("public"))
select package, numberOfTypes