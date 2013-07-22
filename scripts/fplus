#!/bin/bash

# find the location of the script
work_path=$PWD
self=`which "$0" 2>/dev/null`
while [ -L $self ] ; do
    cd $(dirname $self)
    self=$(readlink $self)
    cd $(dirname $self)
    self="$PWD/$(basename $self)"
done
cd $work_path

# the jar should be located next to the script
folder=$(dirname $self)
jarname="${folder}/fplus.jar"
java=java
if test -n "$JAVA_HOME"; then
    java="$JAVA_HOME/bin/java"
fi
$java -jar $jarname "$@"
exit $?
