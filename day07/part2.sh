#!/bin/bash

DU=`du -b fs`

LEFT=70000000
NEEDED=30000000

function get-actual() {
	local path=$1
	local size=$2

	local subdirs=`echo "$DU" | grep "$FILEPATH" | wc -l`
	let subdirs--

	let size-=$subdirs*4096
	if [[ -d $path ]]; then
		let size-=4096
	fi
	echo $size
}

TAKEN=`tail -1 <<< $DU`
FILEPATH=`cut -f 2 <<< $TAKEN`
SIZE=`cut -f 1 <<< $TAKEN`
taken=`get-actual $FILEPATH $SIZE`
let LEFT-=$taken
let NEEDED-=$LEFT

SMALLEST=$taken
while read l; do
        FILEPATH=`cut -f 2 <<< $l`
        SIZE=`cut -f 1 <<< $l`
        filesize=`get-actual $FILEPATH $SIZE`

        if [[ $filesize -ge $NEEDED ]] && [[ $filesize -le $SMALLEST ]]; then
		SMALLEST=$filesize
		echo a >/dev/null
        fi
done <<< $DU

echo "Needed:   $NEEDED"
echo "Smallest: $SMALLEST"
