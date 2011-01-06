http://www.coding-stories.com/2010/09/02/signer-les-jars-avec-maven/#more-266

You need a ${HOME}/.m2/settings.xml file, with:

<settings>
  <profiles>
    <profile>
      <id>tinasign</id>
      <properties>
        <keystore.path>~/Private/prod.keystore</keystore.path>
        <keystore.alias>tinasoft</keystore.alias>
        <keystore.type>JKS</keystore.type>
        <keystore.store.password>${keystore.password}</keystore.store.password>
        <keystore.key.password>${keystore.password}</keystore.key.password>
        <keystore.password>*********</keystore.password>
      </properties>
    </profile>
  </profiles>
</settings>


