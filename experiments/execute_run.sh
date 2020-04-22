#!/bin/bash

#Reading arguements
while getopts ":r:c:" opt; do
	case $opt in
		r) run_id="$OPTARG"
		;;
		c) current_date="$OPTARG"
		\?) echo "Invalid option -$OPTARG" >&2
		;;
	esac
done

echo "$run_id"
Rscript ../lsh_data_processing/covid19_lsh_data_processing.R -c "$current_date" -p "$prediction_date" -d "$path_to_lsh_data" -r "$print_report"
