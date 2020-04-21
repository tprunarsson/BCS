#!/bin/bash

#Reading arguements
while getopts ":r:" opt; do
	case $opt in
		r) run_id="$OPTARG"
		;;
		\?) echo "Invalid option -$OPTARG" >&2
		;;
	esac
done

echo "$run_id"

