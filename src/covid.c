/*
  COVID-19  Population-Quarantine-Isolation-(pre)-Ward-ICU-(ventilator) 
            Discrete Event Simulator 
  author:   tpr@hi.is        
  version:  05/04/2020 (Sunday Release)
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "simlib.h" /* Required for use of simlib.c. */
#include "rndlib.h" /* Required for use of rndlib.c. */
#include "covid.h"  /* all #DEFINEs for the this file */

/* The following is the transition probability matrix from one
   location to the next, length of stay for different agegroups
   and location, and location on first arrival for different age
   groups. */
double CDF[MAX_AGE_GROUPS][RECOVERED-HOME+1][RECOVERED-HOME+1];
double losCDF[MAX_AGE_GROUPS][RECOVERED-HOME+1][MAX_LOS_DAYS];
double firstLocCDF[MAX_AGE_GROUPS][RECOVERED-HOME+1];
double CDFposterior[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];

/* The different locations used */ 
char *szLocations[RECOVERED-HOME+1] = {"HOME","INPATIENT_WARD","INTENSIVE_CARE_UNIT","DEATH","RECOVERED"};

double ProbUnder50; /* this is computed by the function readFirstCDF */
int numRecovered;
int numDeath;
int repeat;
int PatientId; /*??*/

FILE *outfile; /* global file pointers for report writing */

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

/*
   convert string location to equivalent integer value
*/
int get_my_location(char *szlocation) {
  int location;
  if (0 == strcmp(szlocation,"home")) location = HOME;
  else if (0 == strcmp(szlocation,"inpatient_ward")) location = INPATIENT_WARD;
  else if (0 == strcmp(szlocation,"intensive_care_unit")) location = INTENSIVE_CARE_UNIT;
  else if (0 == strcmp(szlocation,"death")) location = DEATH;
  else if (0 == strcmp(szlocation,"recovered")) location = RECOVERED;
  else {
    printf("error: unknown keyword (location) called %s", szlocation);
    exit(1);
  }
  return(location);
}

/* 
  Read covid.hi.is posterior predictions for give day (MAX_SIM_TIME, )
*/
int readHIposteriors(char *fname, char *szDate, int day, double *dbl) { /* TODO name change */
  FILE *fid;
  char sztmp[64];
  char buffer[8192];
  int numdays = 0, tmpday = -100000;
  char *token;
  int i;

  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open covid.hi.is posterior predictions data file %s\n", fname);
    exit(1);
  }
  if (NULL == fgets(buffer, 8192, fid)) /* remove the header! */
    return -1;
  numdays = clear_symbol(buffer,',');
  
  while (NULL != fgets(buffer, 8192, fid)) {
    tmpday++;
    token = strtok(buffer, ",");
    sprintf(sztmp, "%s", token);
   
    if (0 == strcmp(sztmp, szDate)) {
      tmpday = 0;
    }
    if (tmpday == day) {
      for (i = 0; i < numdays; i++) {
        token = strtok(NULL, ",");
        sscanf(token, "%lg", &dbl[i] );
      }
      break;
    }
  }
  if (tmpday < 0) {
    printf("error: did not find poterior day %d starting from date %s\n", day, szDate); 
    exit(1);
  }
  return numdays;
}

/*
  Computes the CDF for the different locations 
*/
int readFirstCDF(char *fname) {
  FILE *fid;
  int age, agegroup, location, under50 = 0, over50 = 0;
  int i, j;
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
    exit(1);
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%d %s %s", &age, szgender, szlocation);
    if (age <= 50) {
      agegroup = 0;
      under50++;
    }
    else {
      agegroup = 1;
      over50++;
    }
    location = get_my_location(szlocation);
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
    fprintf(outfile, "firstLocCDF[%d] = ", i);
    for (j = 0; j < n; j++)
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
    exit(1);
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%s %s %d %d", szlocation, szagegroup, &day, &value);
    if (0 == strcmp(szagegroup,"0-50"))
      agegroup = 0;
    else if (0 == strcmp(szagegroup,"51+"))
      agegroup = 1;
    else {
        printf("error: unknown keyword (age) in %s called %s\n", fname, szagegroup);
        exit(1);
      }
    location = get_my_location(szlocation);
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
  for (i = 0; i < MAX_AGE_GROUPS; i++) {
    fprintf(outfile,"losCDF[%d]= \n", i);
    for (j = 0; j < n; j++) {
      for (k = 0; k < MAX_LOS_DAYS; k++)
        fprintf(outfile, "%.4g ", losCDF[i][j][k]);
      fprintf(outfile,"\n");
    }
  }
  fclose(fid);
  return 0;
}

/* Reading number between states */

int readTransitionP(char *fname, int agegroup) {
  FILE *fid;
  int i, j;
  int n = (RECOVERED - HOME + 1);
  double P[RECOVERED - HOME + 1][RECOVERED - HOME + 1], sum;
  char buffer[1024];
 
  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open transition probability file %s\n", fname);
    exit(1);
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  for (i = 0; i < n; i++) {
    for (j = 0; j < n-1; j++) {
      fscanf(fid, "%lg,", &P[i][j]);
    }
    fscanf(fid, "%lg", &P[i][j]);
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
  fprintf(outfile, "CDF[%d] = \n", agegroup);
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++)
      fprintf(outfile, "%.4g ", CDF[agegroup][i][j]);
    fprintf(outfile, "\n");
  }
  fclose(fid);
  return 0;
}

/*
  A randomly sampled length of stay for the different locations based on an empirical distribution
  defined by the CDF (see function in rndlib.c, note it returns integers 0,1,2,3 and so we add 1.0)
*/
double lengthOfStay(int location, int agegroup, int newlocation, int days_from_diagnosis) {
  double los;
 
  if ((location == HOME) && (newlocation == RECOVERED)) {
    los = discrete_empirical(losCDF[agegroup][location], MAX_LOS_DAYS, STREAM_LOS) + 1.0; 
    while ((days_from_diagnosis+los) < 14)
      los = (double) discrete_empirical(losCDF[agegroup][location], MAX_LOS_DAYS, STREAM_LOS);
  }
  else
    los = discrete_empirical(losCDF[agegroup][location], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
  return los;
}

/*
   Routing heuristic, TODO: check that location != newlocation

  note we dont wanto to go to recovered if days_from_diagnosis < 14

*/
int where_to_next(int agegroup, int location, int days_from_diagnosis, int lastworst) {
  int newlocation;
  int n = RECOVERED - HOME + 1;

  newlocation = discrete_empirical(CDF[agegroup][location], n, STREAM_AGE);
  while ((newlocation == INTENSIVE_CARE_UNIT) && (lastworst == INTENSIVE_CARE_UNIT))
    newlocation = discrete_empirical(CDF[agegroup][location], n, STREAM_AGE);
  return newlocation;
}

/*
  The init_model function reads from a file the current list of infected
  individuals at each location and puts them into a list associated.
  Note that init_simlib() will set global wall clock (sim_time) to zero.
*/
int init_model(char *fname) {
  FILE *fid;
  unsigned int id, count;
  int days_from_diagnosis, dayinloc, location, newlocation, age, agegroup, worstlocation;
  double departureday, los;
  char buffer[1024], szlocation[32], szgender[32], szworstlocation[32];
 
  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("fatal: could not open file %s\n", fname);
    exit(1);
  }
  if (NULL == fgets(buffer, 1024, fid)) /* remove the header! */
    return 1;
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%u %d %s %s %d %d %s", &id, &age, szgender, szlocation, &dayinloc, &days_from_diagnosis, szworstlocation);
    if (age <= 50) 
      agegroup = 0;
    else
      agegroup = 1;
    location = get_my_location(szlocation);
    worstlocation = get_my_location(szworstlocation);
    transfer[ATTR_AGEGROUP] = (double)agegroup;
    transfer[ATTR_DAYSINLOC] = (double)dayinloc;
    transfer[ATTR_DAYSDIAG] = (double)days_from_diagnosis;
    transfer[ATTR_LOCATION] = (double)location;
    transfer[ATTR_LASTWORSTLOC] = (double)worstlocation;
    transfer[ATTR_PERSON] = (double)id;
    newlocation = where_to_next(agegroup, location, days_from_diagnosis, worstlocation);
    /* where should we go next? */
    los = lengthOfStay(location, agegroup, newlocation, days_from_diagnosis) - (double)dayinloc;
    count = 0;
    while (los < 0) {
      los = lengthOfStay(location, agegroup, newlocation, days_from_diagnosis) - (double)dayinloc;
      count++;
      if (count > 100) {
        los = 0.0001;
        break;
      }
    }
    departureday = sim_time + los;
    transfer[ATTR_DAYSDIAG] += los;
    transfer[ATTR_NEXTLOCATION] = (double)newlocation;
    transfer[ATTR_DEPARTDAY] = (double)departureday;
    list_file (INCREASING, location);
    transfer[ATTR_LOCATION] = (double)location;
    event_schedule(departureday, EVENT_DEPARTURE);
  }
  fclose(fid);
  return 0;
}
/*
  The arrive function is used to generate new individuals arriving at sim_time.
  The EVENT_NEW_ARRIVE should be scheduled once every day and the total number of
  newly infected should be based on a prediction model, we use covid.hi.is
*/
void arrive(int n) {
  int i, location, newlocation, agegroup;
  double departureday, los, u;

  for (i = 0; i < n; i++) {
    u = urand (STREAM_AGE);
    agegroup = (u > ProbUnder50);
    /* we need a strategy for selecting which location we enter */
    location = discrete_empirical(firstLocCDF[agegroup], RECOVERED-HOME+1, STREAM_AGE);
    newlocation = where_to_next(agegroup, location, 0, location); /* fresh arrival */
    los = lengthOfStay(location, agegroup, newlocation, 0);
    transfer[ATTR_PERSON] = PatientId++;
    transfer[ATTR_AGEGROUP] = (double)agegroup;
    transfer[ATTR_DAYSDIAG] += los;
    transfer[ATTR_DAYSINLOC] = los;
    transfer[ATTR_LOCATION] = (double)location;
    transfer[ATTR_NEXTLOCATION] = (double)newlocation;
    transfer[ATTR_LASTWORSTLOC] = (double)location;
    departureday = floor(sim_time) + los;
    transfer[ATTR_DEPARTDAY] = departureday;
    if (repeat == 0)
      printf("trace[%d]: Person-%d is entering for %s on day %.2g (worst location is %s, days_from_diagnosis = %.2g)\n", repeat, (int)transfer[ATTR_PERSON], szLocations[location], sim_time, szLocations[location], transfer[ATTR_DAYSDIAG]);
 
    list_file (INCREASING, location);
    transfer[ATTR_LOCATION] = (double)location; /* must be repeated since transfer is new */
    event_schedule(departureday, EVENT_DEPARTURE);
  }
}

/* The departure function removes the individual from the location list and moves the
   patient to a new location based on the cumulative transition probability matrix CDF */
void depart(void) {
  int location, newlocation, newnewlocation, worstlocation;
  int days_from_diagnosis, agegroup, daysinloc;
  unsigned int id;
  double departureday, los;
  
  location = (int)transfer[ATTR_LOCATION]; /* location was in EVENT_LIST's transfer */
  list_remove (FIRST, location); /* now all the properties of this person is in transfer */
  
  agegroup = (int)transfer[ATTR_AGEGROUP];
  days_from_diagnosis = (int)transfer[ATTR_DAYSDIAG];
  daysinloc = (int) transfer[ATTR_DAYSINLOC];
  id = (unsigned int)transfer[ATTR_PERSON];
  newlocation = (int) transfer[ATTR_NEXTLOCATION]; /* was decided when assigned */
  worstlocation = (int) transfer[ATTR_LASTWORSTLOC];
  /* in the future we may want to route undividual patients, then tracing may be useful */
  if (repeat == 0)
    printf("trace[%d]: Person-%d is leaving %s for %s on day %.2g (worst location is %s, days from diagnosis = %d)\n", repeat, id, szLocations[location], szLocations[newlocation], sim_time, szLocations[worstlocation], days_from_diagnosis);
  if (newlocation == RECOVERED) 
    numRecovered++;
  else if (newlocation == DEATH)
    numDeath++;
  else {
    worstlocation = MAX(newlocation, worstlocation); /* ICU with highest value */
    newnewlocation = where_to_next(agegroup, newlocation, days_from_diagnosis, worstlocation );
    los = lengthOfStay(newlocation, agegroup, newnewlocation, days_from_diagnosis);
    if (repeat == 0)
      printf("[trace-x]: plan to stay here for %.0g days\n", los);
    departureday = sim_time + los;
    transfer[ATTR_LASTWORSTLOC] = MAX(newlocation, worstlocation);
    transfer[ATTR_DEPARTDAY] = departureday;
    transfer[ATTR_LOCATION] = (double)newlocation;
    transfer[ATTR_DAYSINLOC] = los;
    transfer[ATTR_DAYSDIAG] += los;
    transfer[ATTR_LASTWORSTLOC] = (double) worstlocation;
    transfer[ATTR_NEXTLOCATION] = (double) newnewlocation;
    list_file (INCREASING, newlocation);
    transfer[ATTR_LOCATION] = (double)newlocation; /* must be repeated since transfer is allocated again */
    event_schedule(departureday, EVENT_DEPARTURE);
  }
}
/* report is used to write to file the current day and the length of all location lists */
void report(FILE *fid, int day, int append) {
  int i;
  if (0 == append) {
    fprintf(fid,"day,home,inpatient_ward,intensive_care_unit,death,recovered\n");
  }
  else {
    fprintf(fid, "%d,", day);
    for (i = HOME; i < (RECOVERED-1); i++) /* note that DEATH and REVOVERED are the last two */
      fprintf(fid, "%d,", list_size[i]);
    fprintf(fid, "%d,%d\n", numDeath, numRecovered);
   }
}

/* 
  The discrete event simulator is a simple Arrival and Departure system.
  A list is maintained for each location in order to collect statistics 
  about the location for each day and about each individual infected.
 */
int main(int argc, char *argv[]) {
 
  int listid, i, j, n;
  char szDate[12], fname[8192];
  char path[1023], path_input[2048], path_output[2048], path_lsh_data[2048];
//  int predMedian[MAX_SIM_TIME], predUpper[MAX_SIM_TIME];
  FILE *statfid;

  maxatr = 15; /* NEVER SET maxatr TO BE SMALLER THAN 4. */

  /* check user input arguments number */
  if (argc < 2) {
    printf("Covid: Discrete Event Simulator (%s)\n", VERSION);
    printf("usage: %s YYYY-MM-DD path\n", argv[0]);
    return 1;
  }
  if (argc < 2)
    strcpy(path, "./");
  if (argc > 1)
    strcpy(path, argv[2]);
  sprintf(path_input, "%s/input/", path);
  sprintf(path_lsh_data,"%s/lsh_data/", path);
  sprintf(path_output,"%s/output/", path);
 
  /* get the date for the simulation run */
  sprintf(szDate,"%s", argv[1]);

  /* The file covid.out is used to report on the setting used for the run */
  sprintf(fname, "%s%s_covid.out", path_output, szDate);
  outfile = fopen (fname, "w");
  
  /* Generate output run file name and pointer */
  sprintf(fname, "%s%s_covid_simulation.csv",path_output, szDate);
  printf(" reading files from %s%s*.csv\n reading files from %s%s*.csv\n writing result in: %s\n and run data into: %s%s_covid.out\n",path_input, szDate, path_lsh_data, szDate, fname, path_output, szDate);
  statfid = fopen (fname, "w");

  /* Initialize simlib */
  init_simlib();

  /* Initialize rndlib */
  init_twister();

  /* Initialize a list for each location from HOMELOC..RECOLOC 
     The list is ranked exactly the same way as the event list INCREASING */
  for (listid = HOME; listid <= RECOVERED; listid++)
    list_rank[listid] = ATTR_DEPARTDAY;

  /* read the length of stay data */
  sprintf(fname, "%s%s_length_of_stay.csv", path_input, szDate);
  readLosP(fname);
  
  /* load first location for new persons arrivals */
  sprintf(fname, "%s%s_first_state.csv", path_lsh_data, szDate);
  readFirstCDF(fname);
  
  /* load posterior predicted values on current date and day number */
  sprintf(fname, "%s%s_iceland_posterior.csv", path_input, szDate);
  fprintf(outfile,"CDFposterior[%s] = \n", szDate);
  for (i = 0; i < MAX_SIM_TIME; i++) {
    n = readHIposteriors(fname, szDate, i, CDFposterior[i]);
    fprintf(outfile,"Day-%d", i);
    for (j = 0; j < n; j++)
      fprintf(outfile,",%.4g", CDFposterior[i][j]);
    fprintf(outfile, "\n");
  }
 
  /* Read the transition probability matrix */
  sprintf(fname, "%s%s_transition_matrix_under_50.csv", path_input, szDate);
  readTransitionP(fname, 0);
  sprintf(fname, "%s%s_transition_matrix_over_50.csv", path_input, szDate);
  readTransitionP(fname, 1);

  report(statfid,0,0); /* write header in output stats file */
  for (repeat = 0; repeat < MAX_REPEAT; repeat++) {
    /* Initialize the model and fire up departure event for thise in system */
    sprintf(fname, "%s%s_current_state.csv", path_lsh_data, szDate);
    init_model(fname);
    event_schedule(0, EVENT_ARRIVAL); /* schedule also new arrivals */
    numRecovered = 0; numDeath = 0; /* zero daily counters for this run */
    PatientId = 0; /* why is this here */
    /* Stop the simulation once the wall clock has reached MAX_SIM_TIME days */
    while ((list_size[LIST_EVENT] != 0) && (sim_time <= MAX_SIM_TIME)) {
    /* Determine the next event. */
      timing();
      switch (next_event_type) {
        case EVENT_ARRIVAL:
          i = discrete_empirical(CDFposterior[(int)floor(sim_time)], MAX_INFECTED_PER_DAY, STREAM_AGE);    
          arrive (i); /* schedule a total number of new arrivals, this value is determined by covid.hi.is model */ 
          event_schedule(sim_time + 1.00001, EVENT_ARRIVAL); /* schedule again new arrivals end of next day */
          /* Write out numbers in each location and zero daily counters */
          report(statfid,(int)floor(sim_time),1);
          numRecovered = 0; numDeath = 0; /* reset global counter for devovered and death */
          break;
        case EVENT_DEPARTURE:
          depart(); /* fire up the departrure event */
          break;
        default:
          break;
      }
    }
    reset_simlib(); /* empty all lists, ready for re-run */
  }
  /* should tidy up simlib memory here! */
  fclose(outfile);
  fclose(statfid);
  return 0;
}