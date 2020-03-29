/*

  COVID-19  Population-Quarantine-Isolation-(pre)-Ward-ICU-(ventilator) 
            Discrete Event Simulator 
  author:   tpr@hi.is        
  version:  27/3/2020

  Notes: the resolution of this simulation is in days, time zero denotes the day today!

 */

#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "simlib.h" /* Required for use of simlib.c. */
#include "rndlib.h" /* Required for use of rndlib.c. */
#include "covid.h"  /* all #DEFINEs for the this file */

/* The following is the transition probability matrix from one
   location to the next. Note that it has a zero on the diagonal
   and uses 10 different agegroups, this variable is global */
double CDF[MAX_AGE_GROUPS][RECOVERED-HOME+1][RECOVERED-HOME+1];
double losCDF[MAX_AGE_GROUPS][RECOVERED-HOME+1][MAX_LOS_DAYS];
double firstLocCDF[MAX_AGE_GROUPS][RECOVERED-HOME+1];

char *szLocations[RECOVERED-HOME+1] = {"HOME","EMERGENCY_ROOM","OUTPATIENT_CLINIC",
                      "INPATIENT_WARD","INTENSIVE_CARE_UNIT","DEATH","RECOVERED"};

double ProbUnder50; /* this is set when we compute the first destination */

FILE *infile, *outfile;

int get_index(char* string, char c) {
    char *e = strchr(string, c);
    if (e == NULL) {
        return -1;
    }
    return (int)(e - string);
}

int readFirstCDF(char *fname) {
  FILE *fid;
  int age, agegroup, location, under50 = 0, over50 = 0;
  int i, j, idx;
  int n = (RECOVERED - HOME + 1);
  double sum;
  char buffer[1024], szgender[128], szlocation[128];
 
  /* zero the losCDF */
  for (i = 0; i < MAX_AGE_GROUPS; i++)
    for (j = 0; j < n; j++)
      firstLocCDF[i][j] = 0.0;

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open length of first historical data file %s\n", fname);
    return 1;
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  while (NULL != fgets(buffer, 1024, fid)) {
    do { /* silly hack to get rid of the commas! */
      idx = get_index(buffer, ',');
      if (idx > 0) buffer[idx] =  ' ';
    } while (idx != -1);
    sscanf(buffer, "%d %s %s", &age, szgender, szlocation);
    if (age <= 50) {
      agegroup = 0;
      under50++;
    }
    else {
      agegroup = 1;
      over50++;
    }
    if (0 == strcmp(szlocation,"home")) location = HOME;
    else if (0 == strcmp(szlocation,"inpatient_ward")) location = INPATIENT_WARD;
    else if (0 == strcmp(szlocation,"intensive_care_unit")) location = INTENSIVE_CARE_UNIT;
    else if (0 == strcmp(szlocation,"death")) location = DEATH;
    else if (0 == strcmp(szlocation,"recovered")) location = RECOVERED;
    else {
      //printf("error: unknown keyword (first) in %s called %s", fname, szlocation);
      location = 0;
      //exit(1);
    }
    firstLocCDF[agegroup][location] = firstLocCDF[agegroup][location] + 1.0;
  }
  for (i = 0; i < MAX_AGE_GROUPS; i++) {
    sum = firstLocCDF[i][0];
    for (j = 1; j < n; j++) {
      sum = sum + firstLocCDF[i][j];
      firstLocCDF[i][j] = firstLocCDF[i][j-1] + firstLocCDF[i][j];
    }
    if (sum > 0) {
      for (j = 0; j < n; j++) {
        firstLocCDF[i][j] = firstLocCDF[i][j]/sum;
      }
    }
  }
  for (i = 0; i < MAX_AGE_GROUPS; i++) {
    for (j = 0; j < n; j++)
      printf("%.4g ",firstLocCDF[i][j]);
    printf("\n");
  }
  ProbUnder50 = (double)under50 / (double)(under50+over50);
  printf("numer of age > 50 is %d and <= 50 %d, probUnder50 = %g\n", over50, under50, ProbUnder50);
  fclose(fid);
  return 0;
}


int readLosP(char *fname) {
  FILE *fid;
  int agegroup, day, location, value;
  int i, j, k, idx;
  int n = (RECOVERED - HOME + 1);
  double sum;
  char buffer[1024], szlocation[128], szagegroup[128];
 
  /* zero the losCDF */
  for (i = 0; i < MAX_AGE_GROUPS; i++)
    for (j = 0; j < n; j++)
      for (k = 0; k < MAX_LOS_DAYS; k++)
        losCDF[i][j][k] = 0.0;

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open length of stay histogram file %s\n", fname);
    return 1;
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  while (NULL != fgets(buffer, 1024, fid)) {
    do { /* silly hack to get rid of the commas! */
      idx = get_index(buffer, ',');
      if (idx > 0) buffer[idx] =  ' ';
    } while (idx != -1);
    sscanf(buffer, "%s %s %d %d", szlocation, szagegroup, &day, &value);
    if (0 == strcmp(szagegroup,"0-50"))
      agegroup = 0;
    else if (0 == strcmp(szagegroup,"51+"))
      agegroup = 1;
    else {
        printf("error: unknown keyword (age) in %s called %s\n", fname, szagegroup);
        exit(1);
      }
    if (0 == strcmp(szlocation,"emergency_room")) location = EMERGENCY_ROOM;
    else if (0 == strcmp(szlocation,"home")) location = HOME;
    else if (0 == strcmp(szlocation,"inpatient_ward")) location = INPATIENT_WARD;
    else if (0 == strcmp(szlocation,"intensive_care_unit")) location = INTENSIVE_CARE_UNIT;
    else if (0 == strcmp(szlocation,"outpatient_clinic")) location = OUTPATIENT_CLINIC;
    else if (0 == strcmp(szlocation,"death")) location = DEATH;
    else if (0 == strcmp(szlocation,"recovered")) location = RECOVERED;
    else {
      printf("error: unknown keyword (location) in %s called %s", fname, szlocation);
      exit(1);
    }
    losCDF[agegroup][location][day] = (double)value;
  }
  for (i = 0; i < MAX_AGE_GROUPS; i++) {
    for (j = 0; j < n; j++){
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
  fclose(fid);
  return 0;
}

int readTransitionP(char *fname, int agegroup) {
  FILE *fid;
  int i, j, ret;
  int n = (RECOVERED - HOME + 1);
  double P[RECOVERED-HOME+1][RECOVERED-HOME+1], sum;
  char buffer[1024];
 
  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open transition probability file %s\n", fname);
    return 1;
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  for (i = 0; i < n; i++) {
    for (j = 0; j < n-1; j++) {
      ret = fscanf(fid, "%lg,", &P[i][j]);
    }
    ret = fscanf(fid, "%lg", &P[i][j]);
    sum = 0.0;
    for (j = 0; j < n; j++) {
      if (i != j)
        sum = sum + P[i][j];
    }
    for (j = 0; j < n; j++) {
      if (sum > 0) {
        P[i][j] = P[i][j]/sum;
        if (i == j)
          P[i][j] = 0.0;
      }
      else
        P[i][j] = 0.0;
    }
    CDF[agegroup][i][0] = P[i][0];
    for (j = 1; j < n; j++) {
      CDF[agegroup][i][j] = CDF[agegroup][i][j-1] + P[i][j];
    }
  }
  /*
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++)
      printf("%.4g ", CDF[agegroup][i][j]);
    printf("\n");
  }
  */
  fclose(fid);
  return 0;
}

/*
  A randomly sampled length of stay for the different locations based on an empirical distribution
  defined by the CDF (see function in rndlib.c, note it returns integers 0,1,2,3 and so we add 1.0)
*/
double lengthOfStay(int location, int agegroup) {
  double los = 0.0;
  los = discrete_empirical(losCDF[agegroup][location], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
    return los;
}

/*
  The init_model function reads from a file the current list of infected
  individuals at each location and puts them into a list associated.
  Note that init_simlib() will set global wall clock (sim_time) to zero.
*/
int init_model(char *fname) {
  FILE *fid;
  int idx;
  unsigned int id;
  int day, dayinloc, location, age, agegroup, gender;
  double departureday;
  char buffer[1024], szlocation[32], szgender[16];
 
  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open file %s\n", fname);
    return 1;
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  while (NULL != fgets(buffer, 1024, fid)) {
    do { /* silly hack to get rid of the commas! */
      idx = get_index(buffer, ',');
      if (idx > 0) buffer[idx] =  ' ';
    } while (idx != -1);
    sscanf(buffer, "%u %d %s %s %d %d", &id, &age, szgender, szlocation, &dayinloc, &day);
    if (age <= 50) 
      agegroup = 0;
    else
      agegroup = 1;
    if (0 == strcmp(szlocation,"emergency_room")) location = EMERGENCY_ROOM;
    else if (0 == strcmp(szlocation,"home")) location = HOME;
    else if (0 == strcmp(szlocation,"inpatient_ward")) location = INPATIENT_WARD;
    else if (0 == strcmp(szlocation,"intensive_care_unit")) location = INTENSIVE_CARE_UNIT;
    else if (0 == strcmp(szlocation,"outpatient_clinic")) location = OUTPATIENT_CLINIC;
    else if (0 == strcmp(szlocation,"death")) location = DEATH;
    else if (0 == strcmp(szlocation,"recovered")) location = RECOVERED;
    else {
      printf("error: unknown keyword (location) in %s called %s", fname, szlocation);
      exit(1);
    }
    if (0 == strcmp(szgender, "Karl")) gender = 2;
    else if (0 == strcmp(szgender, "Kona")) gender = 1;
    else {
      printf("error: unknown gender %s\n", szgender);
      return 1;
    }
    transfer[ATTR_AGEGROUP] = (double)agegroup;
    transfer[ATTR_DAYS] = (double)day;
    transfer[ATTR_GENDER] = (double)gender;
    transfer[ATTR_DAYSINLOC] = (double)dayinloc;
    transfer[ATTR_LOCATION] = (double)location;
    transfer[ATTR_PERSON] = (double)id;
    departureday = lengthOfStay(location, agegroup) - (double)dayinloc;
    departureday = sim_time + MAX(1.0,departureday);
    transfer[ATTR_DEPARTDAY] = (double)departureday;
    list_file (INCREASING, location);
    transfer[ATTR_LOCATION] = (double)location;
    event_schedule(departureday, EVENT_DEPARTURE);
  }
  fclose(fid);
  event_schedule(0, EVENT_ARRIVAL); /* schedule also new arrivals */
  return 0;
}
/*
  The arrive function is used to generate new individuals arriving at sim_time.
  The EVENT_NEW_ARRIVE should be scheduled once every day and the total number of
  newly infected should be based on a prediction model, the ratio of new arrivals are
  at the time of writing in the ratio of: 
  12 ICU, 6 ICU-VENT, 19 RECOVERED, 1 DEAD, 39 WARD, 860 HOME
*/
void arrive(int n) {
  int i;
  /* this is the agegroup distribution CDF 28/3 */
  //double ageCDF[10] = {0.02076843,0.09449637,0.24714434,0.41640706,0.64174455,0.82658359,0.95742471,0.99376947,0.99688474,1.0};
  int day, dayinloc, location, age, agegroup, gender;
  double departureday, u;

  for (i = 0; i < n; i++) {

    u = urand (STREAM_AGE);
    agegroup = (u <= ProbUnder50);
    day = sim_time; /* new arrivals today, that is sim_time wall-clock */
    u = urand (STREAM_AGE);
    gender = (u < 0.5) + 1;
    dayinloc = 0;
    /* we need a strategy for selecting which location we enter */
    location = discrete_empirical(firstLocCDF[agegroup], RECOVERED-HOME+1, STREAM_AGE);
    transfer[ATTR_AGEGROUP] = (double)agegroup;
    transfer[ATTR_DAYS] = (double)day;
    transfer[ATTR_GENDER] = (double)gender;
    transfer[ATTR_DAYSINLOC] = (double)dayinloc;
    transfer[ATTR_LOCATION] = (double)location;
    departureday = sim_time + MAX(1.0,lengthOfStay(location, agegroup) - (double)dayinloc);
    transfer[ATTR_DEPARTDAY] = (double)departureday;
    list_file (INCREASING, location);
    transfer[ATTR_LOCATION] = (double)location; /* must be repeated since transfer is new */
    event_schedule(departureday, EVENT_DEPARTURE);
  }
}

/* The departure function removes the individual from the location list and moves the
   patient to a new location based on some probability measure */
void depart(void) {
  int location, newlocation;
  int day, agegroup, gender;
  unsigned int id;
  double departureday;
  int n = RECOVERED - HOME + 1;
  
  location = (int)transfer[ATTR_LOCATION];
  list_remove (FIRST, location); /* now all the properties of this person is in transfer */
  
  agegroup = (int)transfer[ATTR_AGEGROUP];
  day = (int)transfer[ATTR_DAYS];
  gender = (int)transfer[ATTR_GENDER];
  id = (unsigned int)transfer[ATTR_PERSON];
  /* find a new location for this person, for now we use RECOLOC */
  newlocation = discrete_empirical(CDF[agegroup][location], n, STREAM_AGE);
  if (newlocation == n) {
    printf("warning: stuck in state %d", location);
    newlocation = location; /* basically stuck where it is!!! */
  }
  /* we need to do some routing here */
  while (transfer[newlocation + 10] > 0) { /* this location has been visited before so just stay where you are? */
    if ((location == INTENSIVE_CARE_UNIT) && (newlocation == INPATIENT_WARD))
      break;
    newlocation = discrete_empirical(CDF[agegroup][location], n, STREAM_AGE);
    if ((newlocation == HOME) && (transfer[HOME + 10] > 1))
      newlocation = RECOVERED;
    if ((newlocation == HOME) || (newlocation == DEATH) || (newlocation == RECOVERED))
      break;
  }
  printf("Person-%d is leaving %s for %s on day %.2g\n", id, szLocations[location], szLocations[newlocation], sim_time);
  transfer[ATTR_LOCATION] = (double)newlocation;
  if ((newlocation == RECOVERED) || (newlocation == DEATH))
    departureday = 365; /* that is were not leaving this state */
  else
    departureday = sim_time + MAX(1.0,lengthOfStay(newlocation, agegroup));
  transfer[ATTR_DEPARTDAY] = (double)departureday;
  transfer[newlocation+10] = transfer[newlocation+10] + 1;
  list_file (INCREASING, newlocation);
  transfer[ATTR_LOCATION] = (double)newlocation; /* must be repeated since transfer is new */
  event_schedule(departureday, EVENT_DEPARTURE);
}

void report(append) {
  int i;
  if (0 == append) {
    fprintf(outfile,"home,emergency_room,outpatient_clinic,inpatient_ward,intensive_care_unit,death,recovered\n");
  }
  else {
    for (i = HOME; i < RECOVERED; i++)
      fprintf(outfile, "%d,", list_size[i]);
    fprintf(outfile,"%d\n",list_size[RECOVERED]);
  }
}

/* 
  The discrete event simulator is a simple Arrival and Departure system.
  A list is maintained for each location in order to collect statistics 
  about the location for each day and about each individual infected.
 */
int main(int argc, char *argv[]) {
 
  int listid, i;
  maxatr = 13; /* NEVER SET maxatr TO BE SMALLER THAN 4. */

  /* check user input to main */
  if (argc < (4+MAX_AGE_GROUPS)) {
    printf("Covid: Discrete Event Simulator\n");
    printf("usage: %s persons.csv loshistogram.csv firstdata.csv transition_agegroupX.csv ... \n", argv[0]);
    return 1;
  }

  outfile = fopen ("covid.out", "w");

  /* Initialize simlib */
  init_simlib();

  /* Initialize rndlib */
  init_twister();

  /* Initialize a list for each location from HOMELOC..RECOLOC 
     The list is ranked exactly the same way as the event list INCREASING */
  for (listid = HOME; listid <= RECOVERED; listid++)
    list_rank[listid] = ATTR_DEPARTDAY;

  /* read the length of stay data */
  readLosP(argv[2]);

  /* load first location for new persons arrivals */
  readFirstCDF(argv[3]);

  /* Read the transition probability matrix */
  for (i = 0; i < MAX_AGE_GROUPS; i++)
    readTransitionP(argv[4+i], i);

  /* Initialize the model and fire up departure event for thise in system */
  init_model(argv[1]);

  /* Stop the simulation once the wall clock has reached 7 days 
    or the event list is empty */
  report(0);

  while ((list_size[LIST_EVENT] != 0) && (sim_time <= 7)) {
    /* Determine the next event. */
    timing();
    // for (i = 0; i < maxatr; i++) printf("%.0f ", transfer[i]); printf("\n");
    switch (next_event_type) {
        case EVENT_ARRIVAL:
          //printf("trace: Arrival event at %.2f days\n", sim_time);
          /* add the patient to the home ward */
          //arrive (60); /* schedule a total number of new arrivals, this value is determined by covid.hi.is model */ 
          event_schedule(sim_time + 1.01, EVENT_ARRIVAL); /* schedule again new arrivals next day */
          // here we could write out statistics for the day!
          report(1);
          break;
        case EVENT_DEPARTURE:
          //printf("trace: Departure event at %.2f days\n", sim_time);
          depart();
          break;
    }
  }
//  report();
  fclose(outfile);

  return 0;
}