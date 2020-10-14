#!/bin/bash

#Reading arguements
while getopts ":r:d:s:e:b:h:c:n:apf" opt; do
	case $opt in
		r) run_id="$OPTARG"
		;;
		d) date_data="$OPTARG"
		;;
		s) date_start="$OPTARG"
		;;
		e) date_end="$OPTARG"
		;;
        b) date_prediction="$OPTARG"
        ;;
		h) historical_reset="$OPTARG"
		;;
        c) forecast="$OPTARG"
        ;;
        n) num_sim_days="$OPTARG"
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
date_prediction_default="2020-04-20"
use_scenario_default=""
use_historical_default="0"
forecast_default="hi"
path_default=".."
use_forecast_default=""
num_sim_days_default=20

date_start=${date_start:-"$date_start_default"}
date_end=${date_end:-"$date_end_default"}
date_prediction=${date_prediction:-"$date_prediction_default"}
use_scenario=${use_scenario:-"$use_scenario_default"}
use_historical=${use_historical:-"$use_historical_default"}
forecast=${forecast:-"$forecast_default"}
num_sim_days=${num_sim_days:-"$num_sim_days_defualt"}
path=${path:-"$path_default"}
use_forecast=${use_forecast:-"$use_forecast_default"}

#Tekur langan tíma að keyra, sleppum því
#cd "$path"/lsh_data_processing
#Rscript covid19_lsh_data_processing.R -d "$date_data" -r "$run_id" -c "$forecast" -p "$date_prediction"

#num_sim_days=$(Rscript -e 'args <- commandArgs(trailingOnly = TRUE);print(as.numeric(as.Date(args[2])-as.Date(args[1])))' "$date_start" "$date_end" | awk '{print $2}')
num_sim_days=20

cd src
make clean
make covid
while read experiment_id model prior_info splitting_variable_name splitting_variable_values heuristic_string
 do ./covid -s "$date_start" -i "$experiment_id" -h "$heuristic_string" -v "$splitting_variable_values" -d "$date_data" -n "$num_sim_days" -p "$path" -f "$use_forecast"
done <  ../input/"$date_data"_"$run_id"_run_info.csv
cd ../lsh_data_processing
Rscript ../R/run_evaluation.R -r "$run_id" -d "$date_data" -s "$date_start"
