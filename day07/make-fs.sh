#!/bin/bash

INPUT=${1:-input}

SRC_FILE=`realpath ${BASH_SROUCE:-$0}`
SRC=`dirname $SRC_FILE`
ROOT="$SRC/fs"

[[ -d $ROOT ]] && rm -r $ROOT
mkdir $ROOT
cd fs

while read l; do
	if [[ "$l" == "$ cd "* ]]; then
		DIRNAME=`echo $l | cut -d' ' -f3`
		[[ "$DIRNAME" = "/" ]] && DIRNAME="$ROOT"

		echo cd $DIRNAME
		cd "$DIRNAME"
	elif [[ "$l" == "dir "* ]]; then
		DIRNAME=`echo $l | cut -d' ' -f2`
		echo "mkdir $DIRNAME"
		mkdir $DIRNAME
	elif [[ "$l" =~ ^[0-9]+ ]]; then
		FILESIZE=`echo $l | cut -d' ' -f1`
		FILENAME=`echo $l | cut -d' ' -f2`
		echo fallocate -l "$FILESIZE" "$FILENAME"
		fallocate -l "$FILESIZE" "$FILENAME"
	fi
done < $SRC/$INPUT

cd - >/dev/null
