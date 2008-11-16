#!/bin/bash

# This script is designed to set up the virtual hosting that I do on limbo.mooresystems.com
# It sets up user accounts, DNS, mail, and apache so far
# Andrew Moore <amoore@mooresystems.com> July 11, 2005



# set -x

while getopts "d:u:" opt; do
    case $opt in
        d ) DOMAIN=$OPTARG ;;
	u ) USERNAME=$OPTARG ;;
    esac
done
shift $(($OPTIND - 1))

usage()
{
  echo "Usage:"
  echo "$0 -d <domain> -u <user>"
  exit
}

zonefile()
{
  zonefile=/etc/bind/db.$DOMAIN
  if [ -f $zonefile ] ; then
    echo "zone file $zonefile already exists. Not editing."
    echo ""
    return
  fi

  echo "adding $DOMAIN to bind configuration..."

  echo "\$INCLUDE /etc/bind/db.common" > $zonefile

  echo "
zone \"$DOMAIN\" {
        type master;
        file \"$zonefile\";
};
" >> /etc/bind/named.conf.local

  echo "zone file $zonefile created and added to named.conf."

  # I don't increment the serial number in db.common since this is a new zone.

  echo "restarting bind..."
  /etc/init.d/bind9 restart
  echo "bind has been restarted. Please check /var/log/daemon.log for errors."
  echo ""

}

user()
{
  if [ -d /home/$USERNAME ] ; then
    echo "$USERNAME home already exists. Not adding user"
    echo ""
    return
  fi

  echo "$USERNAME doesn't exist. Let's add the account..."
  # maybe automating this will be better, but for now, just call adduser
  /usr/sbin/adduser

  echo ""

}

postfix()
{

  ODIR=$(pwd)
  cd /etc/postfix

  echo "Preparing to set up postfix for $DOMAIN and $USERNAME..."

  grep -q $DOMAIN /etc/postfix/virtual
  if [ $? -eq 1 ] ; then 
    echo "adding $DOMAIN to /etc/postfix/virtual"
    echo "$DOMAIN           amavis" >> /etc/postfix/virtual  # make this a local domain to amavis
    echo "@$DOMAIN           $USERNAME" >> /etc/postfix/virtual
  else 
    echo "$DOMAIN already in /etc/postfix/virtual:"
    grep $DOMAIN /etc/postfix/virtual
    echo ""
  fi


  grep -q $DOMAIN /etc/postfix/canonical
  if [ $? -eq 1 ] ; then 
    echo "adding $USERNAME@$DOMAIN to /etc/postfix/canonical"
    echo "$USERNAME@mooresystems.com      $USERNAME@$DOMAIN" >> /etc/postfix/canonical
  else 
    echo "$USERNAME@$DOMAIN already in /etc/postfix/canonical:"
    grep $DOMAIN /etc/postfix/canonical
    echo ""
  fi

  # /etc/postfix/Makefile will help you make the *.db hashes
  make
  echo "postfix configured."

  echo "checking postfix configuration:"
  echo "mail to @$DOMAIN will be sent to:"
  postmap -q @$DOMAIN /etc/postfix/virtual

  echo "reloading postfix..."
  make reload
  echo "postfix reloaded. mail to $DOMAIN should go to $USERNAME now"
  echo ""

  cd $ODIR
}

apache()
{

  echo "Configuring Apache..."

  WEBDIR=/home/www/$DOMAIN
  if [ -d $WEBDIR ] ; then
    echo "$WEBDIR already exists."
    echo ""
  else
    mkdir $WEBDIR
    chown -R $USERNAME $WEBDIR
  fi

  WEBCONF=/etc/apache2/sites-available/$DOMAIN
  WEBLOGDIR=/var/log/apache2/$DOMAIN
  if [ -f $WEBCONF ] ; then
    echo "Apache configuration for $DOMAIN already exists"
  else
    IP=$(ifconfig eth0 | grep inet | cut -d: -f 2 | cut -d" " -f 1)
    echo "<VirtualHost $IP:80>
    ServerName  $DOMAIN
    ServerAlias  *.$DOMAIN $DOMAIN
    ServerAdmin $USERNAME@$DOMAIN
    DocumentRoot /home/www/$DOMAIN
    ErrorLog $WEBLOGDIR/error.log
    TransferLog $WEBLOGDIR/access.log
</VirtualHost>" > $WEBCONF
  fi

  if [ -d $WEBLOGDIR ] ; then
    echo "OK: $WEBLOGDIR already exists."
  else
    echo "creating weblog dir: $WEBLOGDIR"
    mkdir $WEBLOGDIR
  fi

  grep -q $DOMAIN /etc/logrotate.d/apache2
  if [ $? -eq 1 ] ; then
    echo "enabling logrote for $DOMAIN"
    echo "$WEBLOGDIR/*.log {
        missingok
        rotate 52
        compress
        delaycompress
        sharedscripts
        postrotate
                if [ -f /var/run/apache2.pid ]; then
                        /etc/init.d/apache2 restart > /dev/null
                fi
        endscript
     }" >> /etc/logrotate.d/apache2
  else
    echo "$DOMAIN logs are already being rotated."
  fi

  if [ -f /etc/apache2/sites-enabled/$DOMAIN ] ; then
    echo "Apache alread configured to serve $DOMAIN"
  else
    ln -s /etc/apache2/sites-available/$DOMAIN /etc/apache2/sites-enabled/
  fi

  /etc/init.d/apache2 restart

}

if [ x$DOMAIN = "x" ] ; then
  usage
fi

if [ x$USERNAME = "x" ] ; then
  usage
fi

# zonefile
# user
postfix
# apache
