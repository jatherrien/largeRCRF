The Java source code for this package can be obtained at https://github.com/jatherrien/largeRCRF-Java, and is also licensed under the GPL-3 license. You can include it with your modifications into the R package by following these steps: 

* Delete all the contents of `inst/java/`
* Build the Java code in its own separate directory using `mvn clean package` (make sure you have Maven installed)
* Copy the `target/largeRCRF-1.0-SNAPSHOT.jar` file produced in the Java directory into `inst/java/`