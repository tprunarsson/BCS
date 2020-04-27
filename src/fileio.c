#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "covid.h"

extern double transitionCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_NUM_STATES];
extern double losCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_LOS_DAYS];
extern double firstSplittingCDF[MAX_NUM_SPLITTING_VALUES];
extern double firstStateCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES];
extern double CDFposterior[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
extern int historicalData[MAXINFECTED][MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES];

extern FILE *outfile;

extern char *szSplittingVariable[MAX_NUM_SPLITTING_VALUES];
extern char *szStateVariable[MAX_NUM_STATES];
extern char *szAllDates[MAX_SIM_TIME][32];

extern person iPerson[MAXINFECTED];
extern person sPerson[MAXINFECTED];
extern int numInfected;
extern int numScenarioInfected;

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

int is_date_before(char *date_str, int comp_year, int comp_month, int comp_day)
{
    int before=0;
    int year, month, day;
    
    scanf(date_str,"%d-%d-%d", &year, &month, &day);
    
    if (year<comp_year)
        before=1;
    else if (month<comp_month)
        before=1;
    else if (day<comp_day)
        before=1;
        
    return before;
}
/* 
  Read additional numbers as possible scenarios
date_diagnosis,initial_state,age_simple,count
2020-04-19,home,age_0-50,49
2020-04-19,home,age_51+,11
2020-04-20,home,age_0-50,57
2020-04-20,home,age_51+,21
*/
/* 
  Read real historical data from the start
*/
int readScenarioData(char *fname, char *szDate, int max_sim_time) {
  FILE *fid;
  char sztmp[64], sztmpdate[32], szstate[128], szsplitting[128];
  char buffer[8192];
  int i, day, state;
  char *token;
  int person_index, fake_person_index = 0, splitting, number;
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
    if (COLS_FILE_SCENARIOS != clear_symbol(buffer,',')) {
      printf("fatal: scenario data file %s corrupt (2)\n", fname);
      exit(1);
    }
    while (NULL != fgets(buffer, 8192, fid)) {
			clear_symbol(buffer,',');
			sscanf(buffer, "%s %s %s %d", sztmp, szstate, szsplitting, &number);
			if (0 == strcmp(sztmp, sztmpdate)) {
				state = get_state(szstate);
				splitting = get_splitting_variable(szsplitting);
        for (i = 0; i < number; i++, fake_person_index++) {
          sPerson[fake_person_index].person_id = FIRST_ADDITIONAL_PERSON_ID - fake_person_index;	// Increment negative numbers
          sPerson[fake_person_index].first_state_indicator[day] = 1;
          sPerson[fake_person_index].real_state[day] = state;
          sPerson[fake_person_index].splitting = splitting;
          strcpy(sPerson[fake_person_index].szDate, sztmpdate);
          sPerson[fake_person_index].start_day = day;
        }
      }
    }
  }
  printf("%d persons found\n", fake_person_index);
  return fake_person_index;
}

/* 
  Read real historical data from szDate onward
*/
int readHistoricalData(char *fname, char *szDate, int max_sim_time) {
  FILE *fid;
  char sztmp[64], sztmpdate[32], szsplitting[32], szstate[128], szworststate[128];
  char buffer[8192];
  int day, days_found, date_found = 0;
  int person_index, person_id, days_in_state, days_from_diagnosis;
  int y, m, d;
  struct tm  t = { 0 };

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open current state file %s\n", fname);
    exit(1);
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
  days_found = 0;
  for (day = 0; day < max_sim_time; day++) {
    t.tm_mday = d;
    t.tm_mon = m - 1;
    t.tm_year = y - 1900;
    t.tm_mday += day;
    mktime(&t);
    strftime(sztmpdate, 32, "%Y-%m-%d", &t);
    rewind(fid); /* read the file from the start */
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
			if (0 == strcmp(sztmp, sztmpdate)) {
				date_found = 1;
        person_index = get_person_index(person_id);
        iPerson[person_index].person_id = person_id;
				strcpy(iPerson[person_index].szDate, sztmpdate);
				// printf("historical data read for person_index=%d, with id=%d\n", person_index, person_id);
				iPerson[person_index].splitting = get_splitting_variable(szsplitting);
				iPerson[person_index].real_state[day] = get_state(szstate);;
     
        if ((day == 0) && (iPerson[person_index].real_state[day] != -1)) {
          iPerson[person_index].first_state_indicator[day] = 1;
        }
        else if ((iPerson[person_index].real_state[day-1] != iPerson[person_index].real_state[day])
                && (iPerson[person_index].real_state[day-1] == -1)) {
          iPerson[person_index].first_state_indicator[day] = 1;
        }
        else
          iPerson[person_index].first_state_indicator[day] = 0;
        
        iPerson[person_index].real_days_in_state[day] = days_in_state;
        iPerson[person_index].real_days_from_diagnosis[day] = days_from_diagnosis;
        iPerson[person_index].real_state_worst[day] = get_state(szworststate);
        iPerson[person_index].start_day = day;
      }
    }
    if (date_found)
      days_found++;
  }
  return days_found;
}

/* 
  Read covid.hi.is posterior predictions for give day (MAX_SIM_TIME, )
  TODO: rewrite this file to scan dates like history read!
*/
int readHIposteriors(char *fname, char *szDate, int day, double *dbl) { /* TODO name change */
  FILE *fid;
  char sztmp[64], sztmpdate[64];
  char buffer[8192];
  int numdays = 0;
  char *token;
  int i = 0, y, m, d;
  struct tm  t = { 0 };

  sscanf(szDate,"%d-%d-%d", &y, &m, &d);
  t.tm_mday = d;
  t.tm_mon = m - 1;
  t.tm_year = y - 1900;
  // Add 'skip' days to the date.                                                               
  t.tm_mday += day;
  mktime(&t);
  strftime(sztmpdate, 32, "%Y-%m-%d", &t);

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open covid.hi.is posterior predictions data file %s\n", fname);
    exit(1);
  }
  if (NULL == fgets(buffer, 8192, fid)) /* remove the header! */
    return -1;
  numdays = clear_symbol(buffer,',');
  for (i = 0; i < numdays; i++) /* make sure this memory is clean in case we don't hvae this date */
    dbl[i] = 0.0;
  dbl[0] = 1.0;  
  
  while (NULL != fgets(buffer, 8192, fid)) {
    token = strtok(buffer, ",");
    sprintf(sztmp, "%s", token);
   
    if (0 == strcmp(sztmp, sztmpdate)) {
      
      for (i = 0; i < numdays; i++) {
        token = strtok(NULL, ",");
        sscanf(token, "%lg", &dbl[i] );
      }
      break;
    }
  }
  if (i == 0) {
    printf("warning: did not find poterior day %d starting from date %s\n", day, sztmpdate); 
  }
  return i;
}

/*
  Computes the CDF for the different states 
*/
int readFirstCDF(char *fname) {
  FILE *fid;
  int id, splitting, state;
  int i, j;
  double sumSplitting,sumState;
  char buffer[1024], szdate[128], szsplitting[128], szstate[128];
 
  /* zero the losCDF */
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++)
    firstSplittingCDF[i] = 0.0;
    for (j = 0; j < MAX_NUM_STATES; j++)
      firstStateCDF[i][j] = 0.0;
      

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open first historical data file %s\n", fname);
    exit(1);
  }
    // RJS check if corrupted
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%d %s %s %s", &id, szsplitting, szdate, szstate); // Read id and date, but is not used yet
    splitting = get_splitting_variable(szsplitting);
    state = get_state(szstate);
    firstSplittingCDF[splitting] = firstSplittingCDF[splitting] + 1.0;
    firstStateCDF[splitting][state] = firstStateCDF[splitting][state] + 1.0;
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
    // RJS: Check if corrupted
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
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

/* Reading number between states */


/* splitting_variable,state,state_tomorrow,count */
int readTransitionCDF(char *fname) {
  FILE *fid;
  int splitting,state_from, state_to;
  int i, j, k, count;
  double P[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_NUM_STATES], sum;
  char buffer[2048], sztmp[128], *token;
 
  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("[fatal error]: could not open state transition count file %s\n", fname);
    exit(1);
  }
  if (NULL == fgets(buffer, 2048, fid)) {
    printf("[fatal error]: state transition by split file %s is corrupt (1)\n", fname);
    exit(1);
  }
  /* count the number of commas */
  if (COLS_FILE_TRANSITIONS-1 != clear_symbol(buffer,',')) {
    printf("[fatal error]: state transition by split file %s is corrupt (2)\n", fname);
    exit(1);
  }
  /* initialize the memory to zero */
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++)
    for (j = 0; j < MAX_NUM_STATES; j++)
      for (k = 0; k < MAX_NUM_STATES; k++)
        P[i][j][k] = 0.0;
  /* scan the entire file for transition counts */
  while (NULL != fgets(buffer, 8192, fid)) {
    token = strtok(buffer, ",");
    sprintf(sztmp, "%s", token);
    splitting = get_splitting_variable(sztmp);
    token = strtok(NULL, ",");
    sscanf(token, "%s", sztmp);
    state_from = get_state(sztmp);
    token = strtok(NULL, ",");
    sscanf(token, "%s", sztmp);
    state_to = get_state(sztmp);
    token = strtok(NULL, ",");
    sscanf(token, "%d", &count);
    P[splitting][state_from][state_to] = count;
  }
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++)
    for (j = 0; j < MAX_NUM_STATES; j++) {
      sum = 0.0;
      for (k = 0; k < MAX_NUM_STATES; k++)
        sum = sum + P[i][j][k];
      if (sum > 0)
        for (k = 0; k < MAX_NUM_STATES; k++)
          P[i][j][k] = P[i][j][k] / sum; 
    }
  for (i = 0; i < MAX_NUM_SPLITTING_VALUES; i++)
    for (j = 0; j < MAX_NUM_STATES; j++) {
      transitionCDF[i][j][0] = P[i][j][0];
      for (k = 1; k < MAX_NUM_STATES; k++)
        transitionCDF[i][j][k] = transitionCDF[i][j][k-1] + P[i][j][k];
    }
  for (k = 0; k < MAX_NUM_SPLITTING_VALUES; k++) {
    fprintf(outfile, "CDF[%s] = \n", szSplittingVariable[k]);
    for (i = 0; i < MAX_NUM_STATES; i++) {
      for (j = 0; j < MAX_NUM_STATES; j++)
        fprintf(outfile, "%.4g ", transitionCDF[k][i][j]);
      fprintf(outfile, "\n");
    }
  }
  fclose(fid);
  return 0;
}
