#! /bin/bash

set -x

cd /tmp

/usr/bin/mysqld_safe --user=root &

until /usr/bin/mysqladmin -u root status > /dev/null 2>&1; do sleep 1; done

# Allow other machines in the docker network to connect, but no others
# NOTE: This is a highly unsecure setup.  This is only for demo purposes.  Don't do this outside of a local demo environment!
echo "CREATE USER 'analytics'@'%' IDENTIFIED BY ''" | mysql -u root
echo "GRANT ALL PRIVILEGES ON *.* TO 'analytics'@'%' WITH GRANT OPTION" | mysql -u root

gunzip ojbc_cc_dimensional_demo.sql.gz

echo "create database ojbc_cc_dimensional_demo" | mysql -u root
mysql -u root ojbc_cc_dimensional_demo < /tmp/ojbc_cc_dimensional_demo.sql
rm -f /tmp/ojbc_cc_dimensional_demo.sql
