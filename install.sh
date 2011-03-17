cd misc
mvn install:install-file -DgroupId=netscape -DartifactId=javascript -Dversion=1.6.0_22 -Dpackaging=jar -Dfile=plugin.jar -DgeneratePom=true
mvn install:install-file -DgroupId=traer -DartifactId=physics -Dversion=3.0 -Dpackaging=jar -Dfile=physics.jar -DgeneratePom=true
mvn install:install-file -DgroupId=org.processing -DartifactId=itext -Dversion=1.0 -Dpackaging=jar -Dfile=itext.jar -DgeneratePom=true
mvn install:install-file -DgroupId=org.processing -DartifactId=pdf -Dversion=1.0 -Dpackaging=jar -Dfile=pdf.jar -DgeneratePom=true
cd ..
