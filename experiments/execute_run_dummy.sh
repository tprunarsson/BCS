#!/bin/bash

#Reading arguements
while getopts ":r:s:p:a" opt; do
	case $opt in
		r) run_id="$OPTARG"
		;;
		s) date_start="$OPTARG"
    ;;
    p) path="$OPTARG"
		;;
		a) use_scenario="-a"
    ;;
		\?) echo "Invalid option -$OPTARG" >&2
		;;
	esac
done
echo "$path"
date_start_default="2020-03-02"
date_end_default="2020-04-20"
use_scenario_default=""
use_historical_default="0"
date_data_default="2020-04-20"

date_start=${date_start:-"$date_start_default"}
date_end=${date_end:-"$date_end_default"}
use_scenario=${use_scenario:-"$use_scenario_default"}
use_historical=${use_historical:-"$use_historical_default"}
num_sim_days=$(Rscript -e 'args <- commandArgs(trailingOnly = TRUE);print(as.numeric(as.Date(args[2])-as.Date(args[1])))' "$date_start" "$date_end" | awk '{print $2}')

Rscript "$path"/experiments/shiny_validation.R -r "$run_id" -s "$date_start" "$use_scenario" -p "$path"
