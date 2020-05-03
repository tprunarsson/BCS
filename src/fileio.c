#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "covid.h"
#include "utils.h"


extern double forecastCDF[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
extern double firstSplittingCDF[MAX_NUM_SPLITTING_VALUES];
extern double firstStateCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES];
extern double transitionCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_LOS_DAYS][MAX_NUM_STATES];
extern double losCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_LOS_DAYS];
extern double	firstScenarioSplittingCDF[MAX_NUM_SPLITTING_VALUES];
extern double firstScenarioStateCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES];

extern FILE *outfile;

extern char *szSplittingVariable[MAX_NUM_SPLITTING_VALUES];
extern char *szStateVariable[MAX_NUM_STATES];
extern char *szAllDates[MAX_SIM_TIME][32];

extern person iPerson[MAXINFECTED];
extern person sPerson[MAXINFECTED];
extern int numInfected;
extern int numScenarioInfected;
extern int iPersonArrivalIndex[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
extern int iPersonArrival[MAX_SIM_TIME];
extern int iPersonCurrentIndex[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
extern int iPersonCurrent[MAX_SIM_TIME];

/* for a workaround with the .csv files, get rid of the commas! */
int get_index(char* string, char c) {
  char *e = strchr(string, c);
  if (e == NULL) {
    return -1;
  }
  return (int)(e - string);
}
int clear_symbol(char *buffer, char s) {
  int idx, i = 0;
  do { 
    idx = get_index(buffer, s);
    if (idx >= 0) {
      buffer[idx] =  ' ';
      i++;
    }
  } while (idx >= 0);
  return i;
}

int get_splitting_variable(char *sz_splitting_variable) {
  int i, splitting_variable = -1;

  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++)
    if (0 == strcmp(sz_splitting_variable,szSplittingVariable[i])) {
      splitting_variable = i;
      break;
    }
  if (splitting_variable < 0) {
    printf("[fatal error]: unknown splitting variable name %s\n", sz_splitting_variable);
    exit(1);
  }
  return(splitting_variable);
}

int get_state(char *sz_state) {
  int i, state = -1;

  for (i = 0; i < MAX_NUM_STATES; i++)
    if (0 == strcmp(sz_state,szStateVariable[i])) {
      state = i;
      break;
    }
  if (state < 0) {
    printf("[fatal error]: unknown state named %s\n available states are: ", sz_state);
    for (i = 0; i < MAX_NUM_STATES-1; i++)
      printf("%s,",szStateVariable[i]);
    printf("%s\n",szStateVariable[i]);
    exit(1);
  }
  return(state);
}

/* scan to find the index in iPerson for person_id */
int get_person_index(int person_id) {
  int i;
  for (i = 0; i < numInfected; i++) {
    if (iPerson[i].person_id == person_id) {
      return i;
    }
  }
  if (numInfected == i) {
    iPerson[i].person_id = person_id;
    numInfected++; /* we have incremented by one individual */
  }
  return i;
}

int get_scenario_person_index(int person_id) {
  int i;
  for (i = 0; i < numScenarioInfected; i++) {
    if (sPerson[i].person_id == person_id) {
      return i;
    }
  }
  return i;
}

/*--------------------  Forecast -------------------------------*/
/* 
  Create CDF for the forecast for each day.
	File header: date, new cases, probability
*/
			
int readForecast(char *fname, char *szStartDate) {
  FILE *fid;
  char buffer[8192], szdate[64];
  int i, j, num_infected, count, sim_day, sumForecast=0;
 
	// zero the CDF
  for (i = 0; i < MAX_SIM_TIME; i++){
		for (j = 0; j < MAX_INFECTED_PER_DAY; j++){
      forecastCDF[i][j] = 0.0;
		}
	}
	
	fid = fopen(fname, "r");
	if (fid == NULL) {
		printf("fatal: could not open forecast data file %s\n", fname);
		exit(1);
	}
	if (NULL == fgets(buffer, 8192, fid)) /* remove the header! */
		 return -1;
	
  if (COLS_FILE_FORECAST-1 != clear_symbol(buffer,',')) {
    printf("[fatal error]: forecast file %s is corrupt (2)\n", fname);
    exit(1);
  }
	
	while (NULL != fgets(buffer, 8192, fid)) {
		clear_symbol(buffer,',');
		sscanf(buffer, "%s %d %d", szdate, &num_infected, &count);
		sim_day=get_sim_day(szdate,szStartDate);

		if (sim_day>=0 && sim_day<MAX_SIM_TIME && num_infected>=0 && num_infected<MAX_INFECTED_PER_DAY){
			forecastCDF[sim_day][num_infected]+=count;
		}
	}
	
  for (i = 0; i < MAX_SIM_TIME; i++) {
    sumForecast = forecastCDF[i][0];
    for (j = 1; j < MAX_INFECTED_PER_DAY; j++) {
      sumForecast += forecastCDF[i][j];
      forecastCDF[i][j] = forecastCDF[i][j-1] + forecastCDF[i][j];
    }
    if (sumForecast > 0) {
      for (j = 0; j < MAX_INFECTED_PER_DAY; j++) {
        forecastCDF[i][j] = forecastCDF[i][j]/sumForecast;
      }
    }
  }
	
  for (i = 0; i < MAX_SIM_TIME; i++) {
    fprintf(outfile, "forecastCDF[%d] = ", i);
    for (j = 0; j < MAX_INFECTED_PER_DAY; j++)
      fprintf(outfile, "%.4g ",forecastCDF[i][j]);
    fprintf(outfile, "\n");
  }
  fclose(fid);
  return 0;
}

/*--------------------  Historical data -------------------------------*/
/*
  Computes the CDF for the different states and splitting values
	File header: initial_state, splitting_variable, count
*/

int readFirstCDF(char *fname) {
  FILE *fid;
  int state, splitting, count;
  int i, j;
  double sumSplitting,sumState;
	char buffer[1024], szstate[128], szsplitting[128];
 
  // zero the CDFs
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++){
    firstScenarioSplittingCDF[i] = 0.0;
		for (j = 0; j < MAX_NUM_STATES; j++){
      firstScenarioStateCDF[i][j] = 0.0;
		}
	}

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open first historical data file %s\n", fname);
    exit(1);
  }

  if (NULL == fgets(buffer, 1024, fid)) // remove the header!
    return 1;
	
	// count the number of commas
  if (COLS_FILE_FIRST_STATE-1 != clear_symbol(buffer,',')) {
    printf("[fatal error]: first state file %s is corrupt (2)\n", fname);
    exit(1);
  }
	
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%s %s %d", szstate, szsplitting, &count);
    splitting = get_splitting_variable(szsplitting);
    state = get_state(szstate);
    firstSplittingCDF[splitting] = firstSplittingCDF[splitting] + (double)count;
    firstStateCDF[splitting][state] = firstStateCDF[splitting][state] + (double)count;
  }
  sumSplitting=0;
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    sumSplitting=sumSplitting + firstSplittingCDF[i];
    firstSplittingCDF[i] = firstSplittingCDF[i-1] + firstSplittingCDF[i];
    sumState = firstStateCDF[i][0];
    for (j = 1; j < MAX_NUM_STATES; j++) {
      sumState = sumState + firstStateCDF[i][j];
      firstStateCDF[i][j] = firstStateCDF[i][j-1] + firstStateCDF[i][j];
    }
    if (sumState > 0) {
      for (j = 0; j < MAX_NUM_STATES; j++) {
        firstStateCDF[i][j] = firstStateCDF[i][j]/sumState;
      }
    }
  }
  if(sumSplitting>0){
    for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    firstSplittingCDF[i] = firstSplittingCDF[i]/sumSplitting;
    }
  }
  fprintf(outfile, "firstSplittingCDF = ");
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    fprintf(outfile, "%.4g ",firstSplittingCDF[i]);
  }
  fprintf(outfile, "\n");
  
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    fprintf(outfile, "firstStateCDF[%d] = ", i);
    for (j = 0; j < MAX_NUM_STATES; j++)
      fprintf(outfile, "%.4g ",firstStateCDF[i][j]);
    fprintf(outfile, "\n");
  }
  fclose(fid);
  return 0;
}


/*
 Computes the CDF for length of stay.
 File header: state,splitting_variable,days,count
*/
int readLosP(char *fname) {
  FILE *fid;
  int splitting, state, days, value;
  int i, j, k;
  double sum;
  char buffer[1024], szsplitting[128], szstate[128];
 
  /* zero the losCDF */
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++)
    for (j = 0; j < MAX_NUM_STATES; j++)
      for (k = 0; k < MAX_LOS_DAYS; k++)
        losCDF[i][j][k] = 0.0;

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open length of stay histogram file %s\n", fname);
    exit(1);
  }

  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
	
	/* count the number of commas */
  if (COLS_FILE_LOS-1 != clear_symbol(buffer,',')) {
    printf("[fatal error]: length of stay file %s is corrupt (2)\n", fname);
    exit(1);
  }
	
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%s %s %d %d", szstate, szsplitting, &days, &value);
    splitting = get_splitting_variable(szsplitting);
    state = get_state(szstate);
    losCDF[splitting][state][days] = (double)value;
  }
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    for (j = 0; j < MAX_NUM_STATES; j++){
      sum = losCDF[i][j][0];
      for (k = 1; k < MAX_LOS_DAYS; k++) {
        sum = sum + losCDF[i][j][k];
        losCDF[i][j][k] = losCDF[i][j][k-1] + losCDF[i][j][k];
      }
      if (sum > 0) {
        for (k = 0; k < MAX_LOS_DAYS; k++) {
          losCDF[i][j][k] = losCDF[i][j][k]/sum;
        }
      }
    }
  }
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    fprintf(outfile,"losCDF[%d]= \n", i);
    for (j = 0; j < MAX_NUM_STATES; j++) {
      for (k = 0; k < MAX_LOS_DAYS; k++)
        fprintf(outfile, "%.4g ", losCDF[i][j][k]);
      fprintf(outfile,"\n");
    }
  }
  fclose(fid);
  return 0;
}

/*
 Calculating CDF for state transitions
 File header: splitting_variable,state,state_duration,state_next,count
*/
int readTransitionCDF(char *fname) {
  FILE *fid;
  int splitting, state, state_next, state_duration;
  int i, j, k,l, count;
  double P[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_LOS_DAYS][MAX_NUM_STATES], sum;
	char buffer[2048], szsplitting[128], szstate[128], szstatenext[128];
 
  /* initialize the memory to zero */
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++)
    for (j = 0; j < MAX_NUM_STATES; j++)
      for (k = 0; k < MAX_LOS_DAYS; k++)
				for (l = 0;l < MAX_NUM_STATES; l++)
					P[i][j][k][l] = 0.0;
	
  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("[fatal error]: could not open state transition count file %s\n", fname);
    exit(1);
  }
	// remove header
  if (NULL == fgets(buffer, 2048, fid)) {
    printf("[fatal error]: state transition by split file %s is corrupt (1)\n", fname);
    exit(1);
  }
  /* count the number of commas */
  if (COLS_FILE_TRANSITIONS-1 != clear_symbol(buffer,',')) {
    printf("[fatal error]: state transition by split file %s is corrupt (2)\n", fname);
    exit(1);
  }
	
  /* scan the entire file for transition counts */
  while (NULL != fgets(buffer, 8192, fid)) {
		clear_symbol(buffer,',');
    sscanf(buffer, "%s %s %d %s %d", szsplitting, szstate, &state_duration, szstatenext, &count);
    splitting = get_splitting_variable(szsplitting);
    state = get_state(szstate);
    state_next = get_state(szstatenext);
    P[splitting][state][state_duration][state_next] = count;
  }
	
	for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++){
		for (j = 0; j < MAX_NUM_STATES; j++){
      for (k = 0; k < MAX_LOS_DAYS; k++){
				sum = 0.0;
				for (l = 0; l < MAX_NUM_STATES; l++){
					sum = sum + P[i][j][k][l];
				}
				if (sum > 0){
					for (l = 0; l < MAX_NUM_STATES; l++){
						P[i][j][k][l] = P[i][j][k][l] / sum;
					}
				}
			}
		}
	}
	for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++){
    for (j = 0; j < MAX_NUM_STATES; j++) {
			for (k = 0; k < MAX_LOS_DAYS; k++){
				transitionCDF[i][j][k][0] = P[i][j][k][0];
				for (l = 1; l < MAX_NUM_STATES; l++){
					transitionCDF[i][j][k][l] = transitionCDF[i][j][k][l-1] + P[i][j][k][l];
				}
			}
    }
	}
	
	// Write to separate file for R
	char fout_name[1024];
	sprintf(fout_name,"../output/2020-03-02_1_transition_cdf.csv"); // RJS Create on the fly
	
	FILE *fout = fopen(fout_name, "w");
	
	fprintf(fout,"splitting_variable,state,state_duration,state_tomorrow,cdf\n");
	
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
		if (szSplittingVariable[i]==NULL)
			break;
    for (j = 0; j < MAX_NUM_STATES; j++) {
			for (k = 0; k < MAX_LOS_DAYS; k++){
				for (l = 0; l < MAX_NUM_STATES; l++){
					fprintf(fout,"%s,%s,%d,%s,%.4g\n", szSplittingVariable[i], szStateVariable[j], k, szStateVariable[l], transitionCDF[i][j][k][l]);
				}
				fprintf(outfile, "\n");
			}
    }
  }
	fclose(fout);
  fclose(fid);
  return 0;
}

/*
  Read historical data from szDate onward
*/
int readHistoricalData(char *fname, char *szDate, int max_sim_time) {
  FILE *fid;
  char sztmp[64], sztmpdate[32], szsplitting[128], szstate[128], szworststate[128], day_before[32];
  char buffer[8192];
  int i, found_day_before, day_before_idx, day, days_found, date_found = 0;
  int person_index, person_id, days_in_state, days_from_diagnosis;
  int y, m, d;
  struct tm  t = { 0 };
	int person_day_before[MAX_INFECTED_PER_DAY];

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open current state file %s\n", fname);
    exit(1);
  }
 
	for (i = 0; i  < MAX_INFECTED_PER_DAY; i ++){
		person_day_before[i] = -1;
	}
	
  for (person_index = 0; person_index < MAXINFECTED; person_index++) {
    for (day = 0; day < max_sim_time; day++) {
        iPerson[person_index].person_id = 0;
        iPerson[person_index].splitting = 0;
        iPerson[person_index].real_state[day] = -1;
        iPerson[person_index].real_state_worst[day] = -1;
        iPerson[person_index].real_days_in_state[day] = -1;
        iPerson[person_index].real_days_from_diagnosis[day] = 0;
        iPerson[person_index].first_state_indicator[day] = 0;
    }
  }
    
  sscanf(szDate,"%d-%d-%d", &y, &m, &d);
	
	t.tm_mday = d - 1;
	t.tm_mon = m - 1;
	t.tm_year = y - 1900;
	mktime(&t);
	strftime(day_before, 32, "%Y-%m-%d", &t);
	day_before_idx = 0;
	
  days_found = 0;
	
  for (day = 0; day < max_sim_time; day++) {
    t.tm_mday = d;
    t.tm_mon = m - 1;
    t.tm_year = y - 1900;
    t.tm_mday += day;
    mktime(&t);
    strftime(sztmpdate, 32, "%Y-%m-%d", &t);
    rewind(fid); /* read the file from the start */
		
		iPersonArrival[day] = 0;	// Initalize the count of arrival each day (first_state).
		iPersonCurrent[day] = 0;	//	Initalize the count of current each day (current_state).
		
    if (NULL == fgets(buffer, 8192, fid)) {
      printf("fatal: historical data file %s corrupt (1)\n", fname);
      exit(1);
    } /* remove the header! */
    if (COLS_FILE_CURRENT_STATE_PER_DATE-1 != clear_symbol(buffer,',')) {
      printf("fatal: current state data file %s corrupt (2)\n", fname);
      exit(1);
    }
    date_found = 0;
		
		while (NULL != fgets(buffer, 8192, fid)) {
			clear_symbol(buffer,',');
			sscanf(buffer, "%d %s %s %s %d %d %s", &person_id, sztmp, szsplitting, szstate, &days_in_state, &days_from_diagnosis, szworststate);
			
			// find those who are in the system the day before start_date (to correctly find those who arrive on start_date)
			if (0 == strcmp(sztmp,day_before)){
				person_day_before[day_before_idx] = person_id;
				day_before_idx++;
			}
			// find those who are in the system from start_date onward
			if (0 == strcmp(sztmp, sztmpdate)) {
				date_found = 1;
        person_index = get_person_index(person_id);
        iPerson[person_index].person_id = person_id;
				strcpy(iPerson[person_index].szDate, sztmpdate);
				// printf("historical data read for person_index=%d, with id=%d\n", person_index, person_id);
				iPerson[person_index].splitting = get_splitting_variable(szsplitting);
				iPerson[person_index].real_state[day] = get_state(szstate);
				
				if (days_in_state == 1){
					if (day > 0) {
						iPerson[person_index].first_state_indicator[day] = (iPerson[person_index].real_state[day-1] == -1) ? 1 : 0;
					} else {
						found_day_before = 0;
						for (i = 0; i < day_before_idx; i++){
							if( iPerson[person_index].person_id == person_day_before[i]){
								found_day_before = 1;
							}
						}
						iPerson[person_index].first_state_indicator[day] = (found_day_before == 0) ? 1 : 0;
					}
				} else {
					iPerson[person_index].first_state_indicator[day] = 0;
				}
	
        iPerson[person_index].real_days_in_state[day] = days_in_state;
        iPerson[person_index].real_days_from_diagnosis[day] = days_from_diagnosis;
        iPerson[person_index].real_state_worst[day] = get_state(szworststate);
        iPerson[person_index].start_day = day;
				
				// Fill the index tables. RJS Need to fix current state before using (need to deal with cases when switching state on day)
				if (iPerson[person_index].real_days_in_state[day] > 1){
					iPersonCurrentIndex[day][iPersonCurrent[day]] = person_index;
					iPersonCurrent[day]++;
				}
				
				if (iPerson[person_index].first_state_indicator[day] == 1){
					iPersonArrivalIndex[day][iPersonArrival[day]] = person_index;
					iPersonArrival[day]++;
				}
      }
		}
		
    if (date_found)
      days_found++;
  }
  return days_found;
}

/* ------------- Scenarios ---------------------*/

/*
  Computes the CDF for the different states and splitting values.
	File header: initial_state, splitting_variable, count
*/
int readScenarioFirstCDF(char *fname) {
  FILE *fid;
  int state, splitting, count;
  int i, j;
  double sumSplitting,sumState;
	char buffer[1024], szstate[128], szsplitting[128];
 
  /* zero the CDFs */
	for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++){
    firstScenarioSplittingCDF[i] = 0.0;
		for (j = 0; j < MAX_NUM_STATES; j++){
      firstScenarioStateCDF[i][j] = 0.0;
		}
	}
	
  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open first_state scenario data file %s\n", fname);
    exit(1);
  }
	
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
	
	/* count the number of commas */
  if (COLS_FILE_FIRST_STATE-1 != clear_symbol(buffer,',')) {
    printf("[fatal error]: length of stay file %s is corrupt (2)\n", fname);
    exit(1);
  }
	
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%s %s %d", szstate, szsplitting, &count);
    splitting = get_splitting_variable(szsplitting);
    state = get_state(szstate);
    firstScenarioSplittingCDF[splitting] = firstScenarioSplittingCDF[splitting] + (double)count;
    firstScenarioStateCDF[splitting][state] = firstScenarioStateCDF[splitting][state] + (double)count;
  }
  sumSplitting=0;
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    sumSplitting=sumSplitting + firstScenarioSplittingCDF[i];
    firstScenarioSplittingCDF[i] = firstScenarioSplittingCDF[i-1] + firstScenarioSplittingCDF[i];
    sumState = firstScenarioStateCDF[i][0];
    for (j = 1; j < MAX_NUM_STATES; j++) {
      sumState = sumState + firstScenarioStateCDF[i][j];
      firstScenarioStateCDF[i][j] = firstScenarioStateCDF[i][j-1] + firstScenarioStateCDF[i][j];
    }
    if (sumState > 0) {
      for (j = 0; j < MAX_NUM_STATES; j++) {
        firstScenarioStateCDF[i][j] = firstScenarioStateCDF[i][j]/sumState;
      }
    }
  }
  if(sumSplitting>0){
    for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    firstScenarioSplittingCDF[i] = firstScenarioSplittingCDF[i]/sumSplitting;
    }
  }
  fprintf(outfile, "firstScenarioSplittingCDF = ");
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    fprintf(outfile, "%.4g ",firstScenarioSplittingCDF[i]);
  }
  fprintf(outfile, "\n");
  
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++) {
    fprintf(outfile, "firstScenarioStateCDF[%d] = ", i);
    for (j = 0; j < MAX_NUM_STATES; j++)
      fprintf(outfile, "%.4g ",firstScenarioStateCDF[i][j]);
    fprintf(outfile, "\n");
  }
  fclose(fid);
  return 0;
}

/*
  Read scenario data from szDate onward
	File header: date_of_diagnosis, count
 RJS: Can be simplified. Don't have to use sPerson. Can read days in one go.
*/
int readScenarioData(char *fname, char *szDate, int max_sim_time) {
  FILE *fid;
  char sztmp[64], sztmpdate[32];
  char buffer[8192];
  int day;
  int person_index, total_person_index=0, count;
  int y, m, d;
  struct tm  t = { 0 };

  fid = fopen(fname, "r");
  if (fid == NULL) {
    return 0;
  }
  else {
    printf(" reading scenario data file %s ... ", fname);
  }

	for (person_index = 0; person_index < MAXINFECTED; person_index++) {
    for (day = 0; day < max_sim_time; day++) {
      sPerson[person_index].person_id = 0;
      sPerson[person_index].splitting = 0;
      sPerson[person_index].real_state[day] = -1;
      sPerson[person_index].real_state_worst[day] = -1;
      sPerson[person_index].real_days_in_state[day] = -1;
      sPerson[person_index].real_days_from_diagnosis[day] = 0;
      sPerson[person_index].first_state_indicator[day] = 0;
    }
  }
  sscanf(szDate,"%d-%d-%d", &y, &m, &d);
  for (day = 0; day < max_sim_time; day++) {
    t.tm_mday = d;
    t.tm_mon = m - 1;
    t.tm_year = y - 1900;
    t.tm_mday += day;
    mktime(&t);
    strftime(sztmpdate, 32, "%Y-%m-%d", &t);
    rewind(fid); /* read the file from the start */
    if (NULL == fgets(buffer, 8192, fid)) {
      printf("fatal: scenario data file %s corrupt (1)\n", fname);
      exit(1);
    } /* remove the header! */
    if (COLS_FILE_SCENARIOS-1 != clear_symbol(buffer,',')) {
      printf("fatal: scenario data file %s corrupt (2)\n", fname);
      exit(1);
    }
    while (NULL != fgets(buffer, 8192, fid)) {
			clear_symbol(buffer,',');
			sscanf(buffer, "%s %d", sztmp, &count);	// state and splitting is set in scenario_arrive.
			if (0 == strcmp(sztmp, sztmpdate)) {
				for (person_index = 0; person_index < count; person_index++,total_person_index++) {
          sPerson[total_person_index].first_state_indicator[day] = 1;
          strcpy(sPerson[total_person_index].szDate, sztmpdate);
          sPerson[total_person_index].start_day = day;
        }
      }
    }
  }
  printf("%d persons found\n", total_person_index);
  return total_person_index;
}
