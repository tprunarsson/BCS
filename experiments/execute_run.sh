#!/bin/bash

#Reading arguements
while getopts ":r:d:s:e:h:ap:f" opt; do
	case $opt in
		r) run_id="$OPTARG"
		;;
		d) date_data="$OPTARG"
		;;
		s) date_start="$OPTARG"
		;;
		e) date_end="$OPTARG"
		;;
		h) historical_reset="$OPTARG"
		;;
		a) use_scenario="-a"
		;;
		p) path="$OPTARG"
		;;
		f) use_forecast="-f"
		;;
		\?) echo "Invalid option -$OPTARG" >&2
		;;
	esac
done
date_start_default="2020-03-02"
date_end_default="$date_data"
use_scenario_default=""
use_historical_default="0"
path_default=".."
use_forecast_default=""

date_start=${date_start:-"$date_start_default"}
date_end=${date_end:-"$date_end_default"}
use_scenario=${use_scenario:-"$use_scenario_default"}
use_historical=${use_historical:-"$use_historical_default"}
path=${path:-"$path_default"}
use_forecast=${use_forecast:-"$use_forecast_default"}

num_sim_days=$(Rscript -e 'args <- commandArgs(trailingOnly = TRUE);print(as.numeric(as.Date(args[2])-as.Date(args[1])))' "$date_start" "$date_end" | awk '{print $2}')

cd "$path"/lsh_data_processing
Rscript covid19_lsh_data_processing.R -d "$date_data" -r "$run_id"

cd ../src
make covid
while read experiment_id model prior_info splitting_variable_name splitting_variable_values heuristic_string
 do ./covid -s "$date_start" -i "$experiment_id" -h "$heuristic_string" -v "$splitting_variable_values" -d "$date_data" -n "$num_sim_days" -p "$path" -r "$use_historical" "$use_scenario" "$use_forecast"
done <  ../input/"$date_data"_"$run_id"_run_info.csv
cd ../lsh_data_processing
Rscript ../R/run_evaluation.R -r "$run_id" -d "$date_data" -s "$date_start"
