************************************************************************
                                  Tinaviz
************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)
************************************************************************

How to setup your development environement:

0. Use Linux or Mac
1. Read Maven manual : http://maven.apache.org/users/index.html
2. Install Maven
4. If it fails, please go to step 1
5. Run "./PLEASE_RUN_ME_THE_FIRST_TIME.sh" 
6. Configure your system for JAR signature :

Explanations in french: http://www.coding-stories.com/2010/09/02/signer-les-jars-avec-maven/#more-266

First, you will need a keystore:

For Development, you can by example put it in ~/dev.keystore

for this, simply type: 

           keytool -genkeypair -dname "cn=David Chavalarias, ou=Tina, o=CNRS, c=FR" -alias devalias -keypass keypass -keystore ~/dev.keystore -storepass storepass -validity 180

For Production:
store path: ~/prod.keystore
store pass: *SOMETHING*

alias: tinasoft
keypass: *SOMETHING_DIFFERENT*

keytool -genkeypair -dname "cn=David Chavalarias, ou=Tinasoft, o=CNRS, c=FR" -alias tinasoft -keypass *SOMETHING_DIFFERENT* -keystore ~/prod.keystore -storepass *SOMETHING* -validity 350


Also, you will need a ${HOME}/.m2/settings.xml file, with a signing profile.

Eg. for a production setting:

<settings>
  <profiles>
    <profile>
      <id>tinasign</id>
      <properties>
        <keystore.path>~/prod.keystore</keystore.path>
        <keystore.alias>tinasoft</keystore.alias>
        <keystore.type>JKS</keystore.type>
        <keystore.store.password>${keystore.password}</keystore.store.password>
        <keystore.key.password>${keystore.password}</keystore.key.password>
        <keystore.password>*SOMETHING*</keystore.password>
      </properties>
    </profile>
  </profiles>
</settings>

7. edit the POM accordingly to your config
8. You are done. Please go to "How to compile"


How to compile :
1. run "mvn install"


Then, all depend on what you want to do.
Maybe you will want to copy the JAR to another directory (eg. tinaweb/js/tinaviz)

