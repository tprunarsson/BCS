#!/bin/bash

#Reading arguements
while getopts ":r:d:s:" opt; do
	case $opt in
		r) run_id="$OPTARG"
		;;
		d) date_data="$OPTARG"
		;;
		s) date_start="$OPTARG"
		;;
		\?) echo "Invalid option -$OPTARG" >&2
		;;
	esac
done
date_start_default="2020-03-02"
date_start=${start_date:-"$date_start_default"}

cd lsh_data_processing
Rscript covid19_lsh_data_processing.R -c "$date_data" -r "$run_id"
cd ..
make src/covid
while read experiment_id model splitting_variable_name splitting_variable_values heuristic_string
 do src/covid "$date_start" . "$model" "$date_data" "$date_data"
done <  input/"$date_data"_"$run_id"_run_info.csv
