Biotool
=======

Notes
=====

This application was built using:
```bash
mvn -B archetype:generate -DarchetypeGroupId=org.apache.maven.archetypes -DgroupId=org.supernifty.biotool -DartifactId=biotool
```

Compiling
=========

```bash
mvn compile
mvn compile-test
mvn test
```

Distributing
============
`mvn package`

Running
=======
Direclty with `java -jar target/biotool-1.0-jar-with-dependencies.jar` or via `biotool.bash` wrapper script.

