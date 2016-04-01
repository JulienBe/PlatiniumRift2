#!/bin/bash

echo "creating map $2"

mkdir "$2"

FULL=`cat $1 | sort | uniq | grep -v 'Error' | grep ':\|+'`
LINKS=`echo $FULL | sed 's/ /\n/g' | grep '+'`
ZONES=`echo $FULL | sed 's/ /\n/g' | grep ':'`

echo "$FULL" > "$2/full"
echo "$LINKS" | sort -nr | sed '/^$/d' > "$2/links"
echo "$ZONES" | sort -nr | sed '/^$/d' > "$2/zones"
echo "$3:$4" > "$2/ids"
