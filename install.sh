cd misc
mvn install:install-file -DgroupId=netscape -DartifactId=javascript -Dversion=1.6.0_22 -Dpackaging=jar -Dfile=plugin.jar -DgeneratePom=true
mvn install:install-file -DgroupId=traer -DartifactId=physics -Dversion=3.0 -Dpackaging=jar -Dfile=physics.jar -DgeneratePom=true
mvn install:install-file -DgroupId=org.processing -DartifactId=itext -Dversion=1.5.1 -Dpackaging=jar -Dfile=itext.jar -DgeneratePom=true
mvn install:install-file -DgroupId=org.processing -DartifactId=pdf -Dversion=1.5.1 -Dpackaging=jar -Dfile=pdf.jar -DgeneratePom=true
mvn install:install-file -DgroupId=org.processing -DartifactId=opengl -Dversion=1.5.1 -Dpackaging=jar -Dfile=opengl.jar -DgeneratePom=true
mvn install:install-file -DgroupId=org.processing -DartifactId=core -Dversion=1.5.1 -Dpackaging=jar -Dfile=core.jar -DgeneratePom=true
mvn install:install-file -DgroupId=org.jocl -DartifactId=jocl -Dversion=0.1.6 -Dpackaging=jar -Dfile=jocl.jar -DgeneratePom=true
mvn install:install-file -DgroupId=codeanticode -DartifactId=clphysics -Dversion=0.0.1 -Dpackaging=jar -Dfile=CLPhysics.jar -DgeneratePom=true
cd ..

