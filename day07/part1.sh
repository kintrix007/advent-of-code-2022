#!/bin/bash

DU=`du -b fs/*`

SUM=0

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

while read l; do
	FILEPATH=`cut -f 2 <<< $l`
	SIZE=`cut -f 1 <<< $l`
	filesize=`get-actual $FILEPATH $SIZE`

	if [[ $filesize -le 100000 ]]; then
		let SUM+=$filesize
	fi
done <<< $DU

echo $SUM
