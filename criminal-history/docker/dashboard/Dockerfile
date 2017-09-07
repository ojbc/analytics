FROM ojbc/mondrian-rest

RUN mkdir /opt/tomcat/sharedcp && \
	sed -i "s/shared.loader=/shared.loader=\/opt\/tomcat\/sharedcp,\/opt\/tomcat\/sharedcp\/*.jar/g" /opt/tomcat/conf/catalina.properties && \
	sed -i "s/jarsToSkip=/jarsToSkip=*.jar,/g" /opt/tomcat/conf/catalina.properties

COPY files/mondrian-connections.json /opt/tomcat/sharedcp
COPY files/MondrianSchema.xml /opt/tomcat/sharedcp
COPY files/mysql-connector-java-5.1.43-bin.jar /opt/tomcat/sharedcp
