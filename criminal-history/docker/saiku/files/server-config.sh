#! /bin/bash

#set -x

apk --update add jq

sed -i -r -e 's/securerandom.source.+/securerandom.source=file:\/dev\/urandom/' /opt/jdk/jdk1.8.0_66/jre/lib/security/java.security

cd /opt/saiku-server
cp start-saiku.sh temp-start-saiku.sh
sed -i "s/\/opt\/saiku-server\/tomcat\/bin\/catalina.sh run/sh startup.sh/g" /opt/saiku-server/temp-start-saiku.sh
./temp-start-saiku.sh


CURLTEST='curl -s -u admin:admin -m 2 http://localhost/saiku/rest/saiku/api/license'

eval $CURLTEST >> /dev/null 2>&1
while [ $? -ne 0 ]; do
       	sleep 2
       	echo "Testing availability of Saiku server..."
   eval $CURLTEST >> /dev/null 2>&1
done

echo "Tomcat successfully started"

curl -sSl -X POST --data 'language=en&username=admin&password=admin' http://localhost/saiku/rest/saiku/session

echo "Removing default/toy data sources and schemas"

curl -sSl -u admin:admin http://localhost/saiku/rest/saiku/admin/datasources | jq -r '.[] | select(.connectionname=="earthquakes")' | grep id | sed -r -e 's/.*: "(.*)",?/curl -sSl -u admin:admin -X DELETE http:\/\/localhost\/saiku\/rest\/saiku\/admin\/datasources\/\1/g' | xargs xargs > /dev/null 2>&1
curl -sSl -u admin:admin http://localhost/saiku/rest/saiku/admin/datasources | jq -r '.[] | select(.connectionname=="foodmart")' | grep id | sed -r -e 's/.*: "(.*)",?/curl -sSl -u admin:admin -X DELETE http:\/\/localhost\/saiku\/rest\/saiku\/admin\/datasources\/\1/g' | xargs xargs > /dev/null 2>&1

curl -sSl -u admin:admin http://localhost/saiku/rest/saiku/admin/schema | jq -r '.[] | select(.name=="foodmart4.xml")' | grep name | sed -r -e 's/.*: "(.*)",?/curl -sSl -u admin:admin -X DELETE http:\/\/localhost\/saiku\/rest\/saiku\/admin\/schema\/\1/g' | xargs xargs > /dev/null 2>&1
curl -sSl -u admin:admin http://localhost/saiku/rest/saiku/admin/schema | jq -r '.[] | select(.name=="earthquakes.xml")' | grep name | sed -r -e 's/.*: "(.*)",?/curl -sSl -u admin:admin -X DELETE http:\/\/localhost\/saiku\/rest\/saiku\/admin\/schema\/\1/g' | xargs xargs > /dev/null 2>&1

echo "Removing default user smith"

curl -sSl -u admin:admin http://localhost/saiku/rest/saiku/admin/users/ | jq -r '.[] | select(.username=="smith")' | grep id | sed -r -e 's/.*: (.*)/curl -sSl -u admin:admin -X DELETE http:\/\/localhost\/saiku\/rest\/saiku\/admin\/users\/\1/g' | xargs xargs > /dev/null 2>&1

echo "Installing Mondrian schemas..."

curl -s -u admin:admin -F name="Analytics" -F file="@/tmp/MondrianSchema.xml;filename=MondrianSchema.xml;type=text/xml" http://localhost/saiku/rest/saiku/admin/schema/Analytics/

echo "Installing data sources..."

curl -s -X POST -u admin:admin --header "Content-Type: application/json" -T /tmp/Analytics.connection.json http://localhost/saiku/rest/saiku/admin/datasources

echo "\nFinal setup:"

echo "\nUsers:"
curl -sSl -u admin:admin http://localhost/saiku/rest/saiku/admin/users/

echo "\nSchemas:"
curl -sSl -u admin:admin http://localhost/saiku/rest/saiku/admin/schema

echo "\nData sources:"
curl -sSl -u admin:admin http://localhost/saiku/rest/saiku/admin/datasources

apk del jq

/opt/saiku-server/stop-saiku.sh

rm temp-start-saiku.sh
