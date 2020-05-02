#!/bin/bash

#Reading arguements
while getopts ":r:d:s:e:" opt; do
	case $opt in
		r) run_id="$OPTARG"
		;;
		d) date_data="$OPTARG"
		;;
		s) date_start="$OPTARG"
		;;
		s) date_end="$OPTARG"
		;;
		\?) echo "Invalid option -$OPTARG" >&2
		;;
	esac
done
date_start_default="2020-03-02"
date_end_default="2020-04-20"
date_start=${date_start:-"$date_start_default"}
date_end=${date_start:-"$date_end_default"}
num_sim_days=$(Rscript -e 'args <- commandArgs(trailingOnly = TRUE);print(as.numeric(as.Date(args[2])-as.Date(args[1])))' "$date_start" "$date_end" | awk '{print $2}')

cd lsh_data_processing
Rscript covid19_lsh_data_processing.R -c "$date_data" -r "$run_id" -s "$date_start"

cd ../src
make covid
while read experiment_id model splitting_variable_name splitting_variable_values heuristic_string
 do ./covid -s "$date_start" -i "$experiment_id" -v "$splitting_variable_values" -d "$date_data" -n 50 -p ..
done <  ../input/"$date_data"_"$run_id"_run_info.csv
