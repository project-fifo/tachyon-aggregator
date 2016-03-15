#!/usr/bin/bash

AWK=/usr/bin/awk
SED=/usr/bin/sed

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
            useradd -g $GROUP -d /data/tachyon -s /bin/false $USER
        fi
        echo Creating directories ...
        mkdir -p /data/tachyon/db
        mkdir -p /data/tachyon/log
        mkdir -p /data/tachyon/etc
        chown -R $USER:$GROUP /data/tachyon
        if [ -d /tmp/tachyon ]
        then
            chown -R $USER:$GROUP /tmp/tachyon/
        fi
        ;;
    POST-INSTALL)
        echo Importing service ...
        svccfg import /opt/local/tachyon/share/tachyon.xml
        CONFFILE=/data/tachyon/etc/tachyon.conf
        RULES=/data/tachyon/etc/tachyon.rules

        cp /opt/local/tachyon/etc/tachyon.conf.example ${CONFFILE}.example
        cp /opt/local/tachyon/etc/tachyon.rules.example ${RULES}.example
        if [ ! -f "${RULES}" ]
        then
            cp ${RULES}.example ${RULES}
        fi
        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
        else
            echo "Please make sure you update your config according to the update manual!"
        fi
        ;;
esac
