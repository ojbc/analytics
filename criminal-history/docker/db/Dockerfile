#
# Unless explicitly acquired and licensed from Licensor under another license, the contents of
# this file are subject to the Reciprocal Public License ("RPL") Version 1.5, or subsequent
# versions as allowed by the RPL, and You may not copy or use this file in either source code
# or executable form, except in compliance with the terms and conditions of the RPL
#
# All software distributed under the RPL is provided strictly on an "AS IS" basis, WITHOUT
# WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY DISCLAIMS ALL SUCH
# WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT. See the RPL for specific language
# governing rights and limitations under the RPL.
#
# http://opensource.org/licenses/RPL-1.5
#
# Copyright 2012-2016 Open Justice Broker Consortium
#

# Docker image that installs mariadb (mysql) and loads the CCH data into the db
# user is saiku

FROM alpine

RUN apk add --update bash mariadb mariadb-client

WORKDIR /usr

RUN bin/mysql_install_db --user=root

RUN sed -i s/skip-networking/#skip-networking/g /etc/my.cnf.d/mariadb-server.cnf

COPY files/server-config.sh /tmp
COPY files/ojbc_analytics_cch_maine.sql.gz /tmp
COPY files/create-views.sql /tmp
RUN chmod a+x /tmp/server-config.sh
RUN /tmp/server-config.sh

CMD ["bin/mysqld_safe", "--user=root"]
