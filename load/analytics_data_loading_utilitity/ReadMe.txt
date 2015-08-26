This project uses Java 7, Karaf 3.0.4 and Camel 2.14.1

This is not compatible with the many of the OJB shared jars however they are not used in this project.

It will need to be built in a separate job in Jenkins configured to use Java 7.

Here are the karaf commands to run:

feature:repo-add camel 2.14.1

feature:install camel
feature:install camel-quartz2
feature:install camel-exec
feature:install camel-mail
install -s mvn:org.apache.servicemix.bundles/org.apache.servicemix.bundles.commons-lang
install -s mvn:joda-time/joda-time/2.6