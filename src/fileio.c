#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "covid.h"

extern double CDF[MAX_SPLITTING_VARIABLE][MAX_STATE_VARIABLE][MAX_STATE_VARIABLE];
extern double losCDF[MAX_SPLITTING_VARIABLE][MAX_STATE_VARIABLE][MAX_LOS_DAYS];
extern double firstLocCDF[MAX_SPLITTING_VARIABLE][MAX_STATE_VARIABLE];
extern double CDFposterior[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
extern int historicalData[MAXINFECTED][MAX_SPLITTING_VARIABLE][MAX_STATE_VARIABLE];

extern FILE *outfile;
extern double ProbUnder50;

extern char *szSplittingVariable[MAX_SPLITTING_VARIABLE];
extern char *szStateVariable[MAX_STATE_VARIABLE];

extern person iPerson[MAXINFECTED];
extern person fPerson[MAXINFECTED];
extern int numInfected;

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

  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++)
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

  for (i = 0; i < MAX_STATE_VARIABLE; i++)
    if (0 == strcmp(sz_state,szStateVariable[i])) {
      state = i;
      break;
    }
  if (state < 0) {
    printf("[fatal error]: unknown state named %s\n available states are: ", sz_state);
    for (i = 0; i < MAX_STATE_VARIABLE-1; i++)
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
  int i, day, location;
  char *token;
  int person_index, fake_person_index = 0, agegroup, number;
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
      fPerson[person_index].person_id = 0;
      fPerson[person_index].age_group = 0;  
      fPerson[person_index].real_location[day] = -1;
      fPerson[person_index].real_location_worst[day] = -1;
      fPerson[person_index].real_days_in_location[day] = -1;
      fPerson[person_index].real_days_from_diagnosis[day] = -1;
      fPerson[person_index].real_days_from_diagnosis[day] = 0;
      fPerson[person_index].real_location[day] = -1;
      fPerson[person_index].first_state_indicator[day] = 0;
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
    if (3 != clear_symbol(buffer,',')) {
      printf("fatal: scenario data file %s corrupt (2)\n", fname);
      exit(1);
    }
    while (NULL != fgets(buffer, 8192, fid)) {
      token = strtok(buffer, ",");
      sscanf(token, "%s", sztmp);
      if (0 == strcmp(sztmp, sztmpdate)) {
        token = strtok(NULL, ",");
        sscanf(token, "%s", szstate);
        location = get_state(szstate);
        token = strtok(NULL, ",");
        sscanf(token, "%s", szsplitting);
        agegroup = get_splitting_variable(szsplitting);
        token = strtok(NULL, ",");
        sscanf(token, "%d", &number);
        for (i = 0; i < number; i++, fake_person_index++) {
          fPerson[fake_person_index].person_id = fake_person_index;
          fPerson[fake_person_index].first_state_indicator[day] = 1;
          fPerson[fake_person_index].real_location[day] = location;
          fPerson[fake_person_index].age_group = agegroup;
          strcpy(fPerson[fake_person_index].szDate, sztmpdate);
          fPerson[fake_person_index].start_day = day;
        }
      }
    }
  }
  printf("%d persons found\n", fake_person_index);
  return fake_person_index;
}

/* 
  Read real historical data from the start
*/
int readHistoricalData(char *fname, char *szDate, int max_sim_time) {
  FILE *fid;
  char sztmp[64], sztmpdate[32], szage[32], szlocation[128], szworstlocation[128];
  char buffer[8192];
  int day, days_found, date_found = 0, location;
  char *token;
  int person_index, person_id, agegroup, days_in_state, days_from_diagnosis;
  int y, m, d;
  struct tm  t = { 0 };

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open historical data file %s\n", fname);
    exit(1);
  }

  for (person_index = 0; person_index < MAXINFECTED; person_index++) {
    for (day = 0; day < max_sim_time; day++) {
      iPerson[person_index].person_id = 0;
      iPerson[person_index].age_group = 0;  
      iPerson[person_index].real_location[day] = -1;
      iPerson[person_index].real_location_worst[day] = -1;
      iPerson[person_index].real_days_in_location[day] = -1;
      iPerson[person_index].real_days_from_diagnosis[day] = -1;
      iPerson[person_index].real_days_from_diagnosis[day] = 0;
      iPerson[person_index].real_location[day] = -1;
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
    if (6 != clear_symbol(buffer,',')) {
      printf("fatal: historical data file %s corrupt (2)\n", fname);
      exit(1);
    }
    date_found = 0;
    while (NULL != fgets(buffer, 8192, fid)) {
      token = strtok(buffer, ",");
      sscanf(token, "%d", &person_id);
      token = strtok(NULL, ",");
      sscanf(token, "%s", sztmp);
      if (0 == strcmp(sztmp, sztmpdate)) {
        date_found = 1;
        person_index = get_person_index(person_id);
        iPerson[person_index].person_id = person_id;
       
       // printf("historical data read for person_index=%d, with id=%d\n", person_index, person_id);
        token = strtok(NULL, ",");
        sscanf(token, "%s", szage);
        agegroup = get_splitting_variable(szage);
        iPerson[person_index].age_group = agegroup;
        token = strtok(NULL, ",");
        sscanf(token, "%s", szlocation);
        location = get_state(szlocation);
        iPerson[person_index].real_location[day] = location;
        if ((day == 0) && (iPerson[person_index].real_location[day] != -1)) {
          iPerson[person_index].first_state_indicator[day] = 1;
        }
        else if ((iPerson[person_index].real_location[day-1] != iPerson[person_index].real_location[day])
                && (iPerson[person_index].real_location[day-1] == -1)) {
          iPerson[person_index].first_state_indicator[day] = 1;
        }
        else
          iPerson[person_index].first_state_indicator[day] = 0;
        strcpy(iPerson[person_index].szDate, sztmpdate);
        token = strtok(NULL, ",");
        sscanf(token, "%d", &days_in_state);
        iPerson[person_index].real_days_in_location[day] = days_in_state;
        token = strtok(NULL, ",");
        sscanf(token, "%d", &days_from_diagnosis);
        iPerson[person_index].real_days_from_diagnosis[day] = days_from_diagnosis;
        token = strtok(NULL, ",");
        sscanf(token, "%s", szworstlocation);
        iPerson[person_index].real_location_worst[day] = get_state(szworstlocation);
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
  Computes the CDF for the different locations 
*/
int readFirstCDF(char *fname) {
  FILE *fid;
  int agegroup, location, under50 = 0, over50 = 0;
  int i, j;
  double sum;
  char buffer[1024], szsplitting[128], szlocation[128];
 
  /* zero the losCDF */
  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++)
    for (j = 0; j < MAX_STATE_VARIABLE; j++)
      firstLocCDF[i][j] = 0.0;

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open first historical data file %s\n", fname);
    exit(1);
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%s %s", szsplitting, szlocation);
    agegroup = get_splitting_variable(szsplitting);
    if (agegroup == 0)
      under50++;
    else
      over50++;
    location = get_state(szlocation);
    firstLocCDF[agegroup][location] = firstLocCDF[agegroup][location] + 1.0;
  }
  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++) {
    sum = firstLocCDF[i][0];
    for (j = 1; j < MAX_STATE_VARIABLE; j++) {
      sum = sum + firstLocCDF[i][j];
      firstLocCDF[i][j] = firstLocCDF[i][j-1] + firstLocCDF[i][j];
    }
    if (sum > 0) {
      for (j = 0; j < MAX_STATE_VARIABLE; j++) {
        firstLocCDF[i][j] = firstLocCDF[i][j]/sum;
      }
    }
  }
  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++) {
    fprintf(outfile, "firstLocCDF[%d] = ", i);
    for (j = 0; j < MAX_STATE_VARIABLE; j++)
      fprintf(outfile, "%.4g ",firstLocCDF[i][j]);
    fprintf(outfile, "\n");
  }
  ProbUnder50 = (double)under50 / (double)(under50+over50);
  fprintf(outfile,"numer of age > 50 is %d and <= 50 %d, probUnder50 = %g\n", over50, under50, ProbUnder50);
  fclose(fid);
  return 0;
}

int readLosP(char *fname) {
  FILE *fid;
  int agegroup, day, location, value;
  int i, j, k;
  double sum;
  char buffer[1024], szlocation[128], szagegroup[128];
 
  /* zero the losCDF */
  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++)
    for (j = 0; j < MAX_STATE_VARIABLE; j++)
      for (k = 0; k < MAX_LOS_DAYS; k++)
        losCDF[i][j][k] = 0.0;

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open length of stay histogram file %s\n", fname);
    exit(1);
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%s %s %d %d", szlocation, szagegroup, &day, &value);
    agegroup = get_splitting_variable(szagegroup);
    location = get_state(szlocation);
    losCDF[agegroup][location][day] = (double)value;
  }
  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++) {
    for (j = 0; j < MAX_STATE_VARIABLE; j++){
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
  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++) {
    fprintf(outfile,"losCDF[%d]= \n", i);
    for (j = 0; j < MAX_STATE_VARIABLE; j++) {
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
  int state_from, state_to, splitting_variable;
  int i, j, k, count;
  double P[MAX_SPLITTING_VARIABLE][MAX_STATE_VARIABLE][MAX_STATE_VARIABLE], sum;
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
  if (3 != clear_symbol(buffer,',')) {
    printf("[fatal error]: state transition by split file %s is corrupt (2)\n", fname);
    exit(1);
  }
  /* initialize the memory to zero */
  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++) 
    for (j = 0; j < MAX_STATE_VARIABLE; j++) 
      for (k = 0; k < MAX_STATE_VARIABLE; k++) 
        P[i][j][k] = 0.0;
  /* scan the entire file for transition counts */
  while (NULL != fgets(buffer, 8192, fid)) {
    token = strtok(buffer, ",");
    sprintf(sztmp, "%s", token);
    splitting_variable = get_splitting_variable(sztmp);
    token = strtok(NULL, ",");
    sscanf(token, "%s", sztmp);
    state_from = get_state(sztmp);
    token = strtok(NULL, ",");
    sscanf(token, "%s", sztmp);
    state_to = get_state(sztmp);
    token = strtok(NULL, ",");
    sscanf(token, "%d", &count);
    P[splitting_variable][state_from][state_to] = count;
  }
  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++) 
    for (j = 0; j < MAX_STATE_VARIABLE; j++) {
      sum = 0.0;
      for (k = 0; k < MAX_STATE_VARIABLE; k++) 
        sum = sum + P[i][j][k];
      if (sum > 0)
        for (k = 0; k < MAX_STATE_VARIABLE; k++)
          P[i][j][k] = P[i][j][k] / sum; 
    }
  for (i = 0; i < MAX_SPLITTING_VARIABLE; i++) 
    for (j = 0; j < MAX_STATE_VARIABLE; j++) {
      CDF[i][j][0] = P[i][j][0];
      for (k = 1; k < MAX_STATE_VARIABLE; k++) 
        CDF[i][j][k] = CDF[i][j][k-1] + P[i][j][k];
    }
  for (k = 0; k < MAX_SPLITTING_VARIABLE; k++) {
    fprintf(outfile, "CDF[%s] = \n", szSplittingVariable[k]);
    for (i = 0; i < MAX_STATE_VARIABLE; i++) {
      for (j = 0; j < MAX_STATE_VARIABLE; j++)
        fprintf(outfile, "%.4g ", CDF[k][i][j]);
      fprintf(outfile, "\n");
    }
  }
  fclose(fid);
  return 0;
}
