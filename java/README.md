Biotool
=======

Notes
=====
This application was built using:
mvn -B archetype:generate -DarchetypeGroupId=org.apache.maven.archetypes -DgroupId=org.supernifty.biotool -DartifactId=biotool

Compiling
=========
mvn compile
mvn compile-test
mvn test

Distributing
============
mvn package

Running
=======
java -jar target/biotool-1.0-jar-with-dependencies.jar
