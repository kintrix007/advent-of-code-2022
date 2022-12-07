#!/bin/bash

DU=`du -b fs/*`

SUM=0

while read l; do
	FILEPATH=`cut -f 2 <<< $l`
	SIZE=`cut -f 1 <<< $l`

	SUBDIRS=`echo "$DU" | grep "$FILEPATH" | wc -l`
	let SUBDIRS--
	#echo -n "$FILEPATH has $SUBDIRS subdirs"

	filesize=$SIZE
	let filesize-=$SUBDIRS*4096
	if [[ -d $FILEPATH ]]; then
		let filesize-=4096
		#echo -n " and is a dir"
	fi
	echo

	if [[ $filesize -le 100000 ]]; then
		let SUM+=$filesize
	fi
done <<< $DU

echo $SUM
