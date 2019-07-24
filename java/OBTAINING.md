The Java source code for this package can be obtained at https://github.com/jatherrien/largeRCRF-Java, and is also licensed under the GPL-3 license. You can include it with your modifications into the R package by following these steps: 

* Delete the Jar file in `inst/java/`
* Build the Java code in its own separate directory using `mvn clean package` in the root of the directory (same folder containing `README.md`). Make sure you have [Maven](https://maven.apache.org/) installed.
* Copy the `library/target/largeRCRF-library-1.0-SNAPSHOT.jar` file produced in the Java directory into `inst/java/`