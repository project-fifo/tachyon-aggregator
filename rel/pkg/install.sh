#!/usr/bin/bash

USER=tachyon
GROUP=$USER

case $2 in
    PRE-INSTALL)
        if grep "^$GROUP:" /etc/group > /dev/null 2>&1
        then
            echo "Group already exists, skipping creation."
        else
            echo Creating tachyon group ...
            groupadd $GROUP
        fi
        if id $USER > /dev/null 2>&1
        then
            echo "User already exists, skipping creation."
        else
            echo Creating tachyon user ...
            useradd -g $GROUP -d /var/db/tachyon -s /bin/false $USER
        fi
        echo Creating directories ...
        mkdir -p /var/db/tachyon
        chown -R $USER:$GROUP /var/db/tachyon
        chown -R $USER:$GROUP /var/log/tachyon
        if [ -d /tmp/tachyon ]
        then
            chown -R $USER:$GROUP /tmp/tachyon/
        fi
        ;;
    POST-INSTALL)
        echo Importing service ...
        svccfg import /opt/local/tachyon/share/tachyon.xml
        CONFFILE=/opt/local/tachyon/etc/tachyon.conf
        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
        else
            echo "Merging old file with new template, the original can be found in ${CONFFILE}.old."
            /opt/local/tachyon/share/update_config.sh ${CONFFILE}.example ${CONFFILE} > ${CONFFILE}.new &&
                mv ${CONFFILE} ${CONFFILE}.old &&
                mv ${CONFFILE}.new ${CONFFILE}
        fi
        ;;
esac
