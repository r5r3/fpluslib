#/bin/bash
set -e

# right host?
HOST=$(hostname -s)
if [ "$HOST" != "harmattan" ] ; then
	echo "ERROR: use this script on harmattan!"
fi

# delete the old documentation
rm -rf /work/trac/agfink.doxygen/fpluslib/html

# copy the new documentation
cp -r html /work/trac/agfink.doxygen/fpluslib/.
