#!/bin/bash

echo "creating map $2"

mkdir "$2"

FULL=`cat $1 | sort | uniq | grep -v 'Error' | grep ':\|+'`
LINKS=`echo $FULL | sed 's/ /\n/g' | grep '+'`
ZONES=`echo $FULL | sed 's/ /\n/g' | grep ':'`

echo "$FULL" > "$2/full"
echo "$LINKS" > "$2/links"
echo "$ZONES" > "$2/zones"
touch "$2/$3:$4"
