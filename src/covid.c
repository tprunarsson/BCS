/*
  COVID-19  Population-Quarantine-Isolation-(pre)-Ward-ICU-(ventilator) 
            Discrete Event Simulator 
  author:   tpr@hi.is        
  version:  10/04/2020 (Good Friday Release)
 */

//#define TRACE
//#define DEBUG
//#define STATS

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "simlib.h" /* Required for use of simlib.c. */
#include "rndlib.h" /* Required for use of rndlib.c. */
#include "covid.h"  /* all #DEFINEs for the this file */
#include "fileio.h" /* all reading/writing of files located here */
#include "utils.h"  /* utility function used */

/* define the different splitting variable and new states */
char *szSplittingVariable[MAX_SPLITTING_VARIABLE] = {"age_0-50", "age_51+"};
char *szStateVariable[MAX_STATE_VARIABLE] = {"home", "home-green", "home-red", "inpatient_ward", "inpatient_ward-green", "inpatient_ward-red", "intensive_care_unit", "intensive_care_unit-green","intensive_care_unit-red", "death", "recovered"};

/* Define the person with all of its properties in a structure */
person iPerson[MAXINFECTED];
person fPerson[MAXINFECTED];

int numInfected = 0;

/* The following is the transition probability matrix from one
   location/state to the next, length of stay for different agegroups
   and location, and location/state on first arrival for different age
   groups. */
double transitionCDF[MAX_SPLITTING_VARIABLE][MAX_STATE_VARIABLE][MAX_STATE_VARIABLE];
double losCDF[MAX_SPLITTING_VARIABLE][MAX_STATE_VARIABLE][MAX_LOS_DAYS];
double firstSplittingCDF[MAX_SPLITTING_VARIABLE];
double firstStateCDF[MAX_SPLITTING_VARIABLE][MAX_STATE_VARIABLE];
double CDFposterior[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
int historicalData[MAXINFECTED][MAX_SPLITTING_VARIABLE][MAX_STATE_VARIABLE];

/* count statistics for analysing the dynamics of the model */
double countStats[MAX_SIM_TIME+1][MAX_STATE_VARIABLE];
double countStatsFalse[MAX_SIM_TIME+1][MAX_STATE_VARIABLE];
double countNumber[MAX_SIM_TIME+1][MAX_STATE_VARIABLE];
double countNumberFalse[MAX_SIM_TIME+1][MAX_STATE_VARIABLE];

/* The real dates for each day of the simulation */ 
char szAllDates[MAX_SIM_TIME][32];

int numRecovered;
int numDeath;
int repeat;
int PatientId; /* used for trace */
int availHistory, availTrueStateDays, availScenarios;

FILE *outfile; /* global file pointers for report writing */

/*
  A randomly sampled length of stay for the different states based on an empirical distribution
  defined by the CDF (see function in rndlib.c, note it returns integers 0,1,2,3 and so we add 1.0)
*/
double lengthOfStay(int splitting, int state, int days_from_diagnosis, int new_state) {
  double length_of_stay;
 
  if (((state == HOME_RED) || (state == HOME_GREEN) || (state == HOME)) && (new_state == RECOVERED)) {
    length_of_stay = discrete_empirical(losCDF[splitting][state], MAX_LOS_DAYS, STREAM_LOS) + 1.0; 
    while ((days_from_diagnosis+length_of_stay) < 14)
      length_of_stay = (double) discrete_empirical(losCDF[splitting][state], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
  }
  else
    length_of_stay = discrete_empirical(losCDF[splitting][state], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
  return length_of_stay;
}

/*
   Routing heuristic given current state and previous worst state
*/
int whereToNext(int splitting, int state, int days_from_diagnosis, int worst_state) {
  int new_state;

  new_state = discrete_empirical(transitionCDF[splitting][state], MAX_STATE_VARIABLE, STREAM_TRANSITION);

  /* this first check will only activate in case we are using the simple model */
  while ( (new_state == INTENSIVE_CARE_UNIT) && (worst_state == INTENSIVE_CARE_UNIT) )
    new_state = discrete_empirical(transitionCDF[splitting][state], MAX_STATE_VARIABLE, STREAM_TRANSITION);

  /* this second check will only activate when we are using the clinical assessment */
  while ( (new_state == INTENSIVE_CARE_UNIT_RED) && (worst_state == INTENSIVE_CARE_UNIT_RED) )
    new_state = discrete_empirical(transitionCDF[splitting][state], MAX_STATE_VARIABLE, STREAM_TRANSITION);

  return new_state;
}

/*
  The init_model function reads from a file the current list of infected
  individuals in each state and puts them into a list associated.
  Note that init_simlib() will set global wall clock (sim_time) to zero.
*/
int init_model(char *fname) {
  FILE *fid;
  int person_id, count, day;
  int days_from_diagnosis, days_in_state, state, new_state, splitting, worst_state, person_idx;
  double departure_day, length_of_stay;
  char buffer[1024], szstate[32], szsplitting[32], szworststate[32];
 
  fid = fopen(fname, "r");
  if (fid == NULL) {
    printf("[fatal error]: could not open file %s for init_mode() \n", fname);
    exit(1);
  }
  if (NULL == fgets(buffer, 1024, fid)) {
    printf("[fatal error]: in init_model() the file %s seems to be corrupt (1)\n", fname);
    exit(1);
  }   
  if (COLS_FILE_CURRENT_STATE-1 != clear_symbol(buffer,',')) {
    printf("[fatal error]: in init_model() the file %s seems to be corrupt (2)\n", fname);
    exit(1);
  }
  day = (int)floor(sim_time);
  while (NULL != fgets(buffer, 1024, fid)) {
    clear_symbol(buffer,',');
    sscanf(buffer, "%d %s %s %d %d %s", &person_id, szsplitting, szstate, &days_in_state, &days_from_diagnosis, szworststate);
    splitting = get_splitting_variable(szsplitting);
    state = get_state(szstate);
    worst_state = get_state(szworststate);
    transfer[ATTR_SPLITTING] = (double)splitting;
    transfer[ATTR_DAYSINSTATE] = (double)days_in_state;
    transfer[ATTR_DAYSDIAGNOSIS] = (double)days_from_diagnosis;
    transfer[ATTR_STATE] = (double)state;
    transfer[ATTR_WORSTSTATE] = (double)worst_state;
    transfer[ATTR_PERSON] = (double)person_id;
    person_idx = get_person_index(person_id);
    new_state = whereToNext(splitting, state, days_from_diagnosis, worst_state);
    /* where should we go next? */
    length_of_stay = lengthOfStay(splitting, state, days_from_diagnosis, new_state) - (double)days_in_state;
    count = 0;
    while (length_of_stay < 0) {
      length_of_stay = lengthOfStay(splitting, state, days_from_diagnosis, new_state) - (double)days_in_state;
      count++;
      if (count > 1000) {
        length_of_stay = 1.0;
        break;
      }
    }
    departure_day = floor(sim_time) + length_of_stay + 0.5;
    transfer[ATTR_DAYSDIAGNOSIS] += length_of_stay;
    transfer[ATTR_NEXTSTATE] = (double)new_state;
    transfer[ATTR_DEPARTDAY] = (double)departure_day;
    iPerson[person_idx].splitting = splitting;
    iPerson[person_idx].real_days_in_state[day] = length_of_stay;
    iPerson[person_idx].real_days_from_diagnosis[day] = days_from_diagnosis + length_of_stay;
    iPerson[person_idx].real_state[day] = state;
    iPerson[person_idx].real_state_worst[day] = worst_state;
    #ifdef STATS
    setSimulationLocation(person_id, (int)floor(sim_time), length_of_stay, state);
    #endif
    list_file (INCREASING, state);
    transfer[ATTR_STATE] = (double)state;
    event_schedule(departure_day, EVENT_DEPARTURE);
  }
  fclose(fid);
  return 0;
}

/*
  The init_model function using real historical data, from given day
*/
int real_reinit_model(int day) {
  int i, k, count, splitting, state, days_in_state, days_from_diagnosis, new_state, worst_state, reset_model = 0;
  double length_of_stay, departure_day;

  for (i = 0; i < numInfected; i++) {
    if (iPerson[i].real_state[day] >= 0) {
      if (iPerson[i].person_id == 0)
        printf("Infected person %d found infected number %d\n", iPerson[i].person_id, i);
      if (0 == reset_model) {
        for (k = 0; k <= MAX_LIST; k++) {
          while (list_size[k] > 0) {
            list_remove(FIRST, k);
          }
        }
        reset_model = 1;
      }      
      splitting = iPerson[i].splitting;
      transfer[ATTR_SPLITTING] = (double)splitting;
      days_in_state = iPerson[i].real_days_in_state[day];
      transfer[ATTR_DAYSINSTATE] = (double)days_in_state;
      days_from_diagnosis = iPerson[i].real_days_from_diagnosis[day];
      transfer[ATTR_DAYSDIAGNOSIS] = (double)days_from_diagnosis;
      state = iPerson[i].real_state[day];
      transfer[ATTR_STATE] = (double)state;
      worst_state = iPerson[i].real_state_worst[day];
      transfer[ATTR_WORSTSTATE] = (double)worst_state;
      transfer[ATTR_PERSON] = (double)iPerson[i].person_id;
      new_state = whereToNext(splitting, state, days_from_diagnosis, worst_state);
      /* where should we go next? */
      length_of_stay = lengthOfStay(splitting, state, days_from_diagnosis, new_state) - (double)days_in_state;
      count = 0;
      while (length_of_stay < 0) {
        length_of_stay = lengthOfStay(splitting, state, days_from_diagnosis, new_state) - (double)days_in_state;
        count++;
        if (count > 1000) {
          length_of_stay = 1.0;
          break;
       }
      }
      departure_day = floor(sim_time) + length_of_stay + 0.5;
      transfer[ATTR_DAYSDIAGNOSIS] += length_of_stay;
      transfer[ATTR_NEXTSTATE] = (double)new_state;
      transfer[ATTR_DEPARTDAY] = (double)departure_day;
      #ifdef STATS
      setSimulationLocation(iPerson[i].person_id, (int)floor(sim_time), lenght_of_stay, state);
      #endif
      list_file (INCREASING, state);
      transfer[ATTR_STATE] = (double)state;
      event_schedule(departure_day, EVENT_DEPARTURE);
    }
  }
  return reset_model;
}

/*
  The init_model function using real historical data, from given day
 NOTE: Change comment
*/
int real_arrive_with_id(int day) {
  int i, splitting, state;
  int new_state;
  double length_of_stay, departure_day;

  for (i = 0; i < numInfected; i++) {
    if (iPerson[i].first_state_indicator[day] == 1) {
      transfer[ATTR_PERSON] = (double)iPerson[i].person_id;
      splitting = iPerson[i].splitting;
      /* we need a strategy for selecting which location we enter */
      state = iPerson[i].real_state[day];
      new_state = whereToNext(splitting, state, 0, state); /* fresh arrival */
      length_of_stay = lengthOfStay(splitting, state, 0, new_state);
      transfer[ATTR_SPLITTING] = (double)splitting;
      transfer[ATTR_DAYSDIAGNOSIS] = length_of_stay;
      transfer[ATTR_DAYSINSTATE] = length_of_stay;
      transfer[ATTR_STATE] = (double)state;
      transfer[ATTR_NEXTSTATE] = (double)new_state;
      transfer[ATTR_WORSTSTATE] = (double)state;
      departure_day = sim_time + length_of_stay;
      transfer[ATTR_DEPARTDAY] = departure_day;
      #ifdef TRACE
      if (repeat == 1)
        printf("trace[%d]: Person-%d is entering for %s on day %.2g (worst location is %s, days_from_diagnosis = %.2g)\n", repeat, (int)transfer[ATTR_PERSON], szStateVariable[location], sim_time, szStateVariable[location], transfer[ATTR_DAYSDIAG]);
      #endif
      #ifdef STATS
      setSimulationLocation(iPerson[i].person_id, (int)floor(sim_time), length_of_stay, state);
      #endif
      list_file (INCREASING, state);
      transfer[ATTR_STATE] = (double)state; /* must be repeated since transfer is new */
      event_schedule(departure_day, EVENT_DEPARTURE);
    }
  }
  return 0;
}

/*
  The arrive function is used to generate new individuals arriving at sim_time.
  The EVENT_NEW_ARRIVE should be scheduled once every day and the total number of
  newly infected should be based on a prediction model, we use covid.hi.is
*/
void arrive(int n) {
  int i, first_state, new_state, splitting;
  double departure_day, length_of_stay;

  for (i = 0; i < n; i++) {
    splitting=discrete_empirical(firstSplittingCDF,MAX_SPLITTING_VARIABLE,STREAM_SPLITTING);
    /* we need a strategy for selecting which location we enter */
    first_state = discrete_empirical(firstStateCDF[splitting], MAX_STATE_VARIABLE, STREAM_FIRST_STATE);
    new_state = whereToNext(splitting, first_state, 0, first_state); /* fresh arrival */
    length_of_stay = lengthOfStay(splitting, first_state, 0, new_state);
    transfer[ATTR_PERSON] = (double)--PatientId; /* dummy ID for prediced patients IDs */
    transfer[ATTR_SPLITTING] = (double)splitting;
    transfer[ATTR_DAYSDIAGNOSIS] += length_of_stay;
    transfer[ATTR_DAYSINSTATE] = length_of_stay;
    transfer[ATTR_STATE] = (double)first_state;
    transfer[ATTR_NEXTSTATE] = (double)new_state;
    transfer[ATTR_WORSTSTATE] = (double)first_state;
    departure_day = sim_time + length_of_stay;
    transfer[ATTR_DEPARTDAY] = departure_day;
    #ifdef TRACE
    if (repeat == 1)
      printf("trace[%d]: Person-%d is entering for %s on day %.2g (worst location is %s, days_from_diagnosis = %.2g)\n", repeat, (int)transfer[ATTR_PERSON], szStateVariable[location], sim_time, szStateVariable[location], transfer[ATTR_DAYSDIAG]);
    #endif
    list_file (INCREASING, first_state);
    transfer[ATTR_STATE] = (double)first_state; /* must be repeated since transfer is new */
    event_schedule(departure_day, EVENT_DEPARTURE);
  }
}

/*
  The arrive function is used to generate new individuals arriving at sim_time.
  
void real_arrive(int day) {

  int i, agegroup, location;
  int newlocation;
  double los, departureday, u;

  for (i = 0; i < numInfected; i++) {
    if (iPerson[i].first_state_indicator[day] == 1) {
      transfer[ATTR_PERSON] = (double)iPerson[i].person_id;
      u = urand (STREAM_AGE);
      agegroup = (u > ProbUnder50);
      //agegroup = iPerson[i].age_group;
      //we need a strategy for selecting which location we enter 
      location = discrete_empirical(firstLocCDF[agegroup], MAX_STATE_VARIABLE, STREAM_AGE);
      //location = iPerson[i].real_location[day];
      newlocation = where_to_next(agegroup, location, 0, location); // fresh arrival
      los = lengthOfStay(splitting, state, 0, new_state);
      transfer[ATTR_AGEGROUP] = (double)agegroup;
      transfer[ATTR_DAYSDIAG] = los;
      transfer[ATTR_DAYSINLOC] = los;
      transfer[ATTR_LOCATION] = (double)location;
      transfer[ATTR_NEXTLOCATION] = (double)newlocation;
      transfer[ATTR_LASTWORSTLOC] = (double)location;
      departureday = sim_time + los;
      transfer[ATTR_DEPARTDAY] = departureday;
      #ifdef TRACE
      if (repeat == 1)
        printf("trace[%d]: Person-%d is entering for %s on day %.2g (worst location is %s, days_from_diagnosis = %.2g)\n", repeat, (int)transfer[ATTR_PERSON], szStateVariable[location], sim_time, szStateVariable[location], transfer[ATTR_DAYSDIAG]);
      #endif
      #ifdef STATS
      setSimulationLocation(iPerson[i].person_id, (int)floor(sim_time), los, location);
      #endif
      list_file (INCREASING, location);
      transfer[ATTR_LOCATION] = (double)location; // must be repeated since transfer is new
      event_schedule(departureday, EVENT_DEPARTURE);
    }
  }
}
*/
      
/*
  The scenario arrive function is used to generate new individuals arriving at sim_time.
  */
void fake_arrive(int day) {

  int i, splitting, first_state;
  int new_state;
  double length_of_stay, departure_day;

  for (i = 0; i < availScenarios; i++) {
    if (fPerson[i].first_state_indicator[day] == 1) {
      transfer[ATTR_PERSON] = (double)fPerson[i].person_id;
      splitting = fPerson[i].splitting;
      first_state = fPerson[i].real_state[day];
      new_state = whereToNext(splitting, first_state, 0, first_state); // fresh arrival
      length_of_stay = lengthOfStay(splitting, first_state, 0, new_state);
      transfer[ATTR_SPLITTING] = (double)splitting;
      transfer[ATTR_DAYSDIAGNOSIS] = length_of_stay;
      transfer[ATTR_DAYSINSTATE] = length_of_stay;
      transfer[ATTR_STATE] = (double)first_state;
      transfer[ATTR_NEXTSTATE] = (double)new_state;
      transfer[ATTR_WORSTSTATE] = (double)first_state;
      departure_day = sim_time + length_of_stay;
      transfer[ATTR_DEPARTDAY] = departure_day;
      #ifdef TRACE
      if (repeat == 1)
        printf("trace[%d]: fakePerson-%d is entering for %s on day %.2g (worst state is %s, days_from_diagnosis = %.2g)\n", repeat, (int)transfer[ATTR_PERSON], szStateVariable[location], sim_time, szStateVariable[location], transfer[ATTR_DAYSDIAG]);
      #endif
      #ifdef STATS
      setSimulationLocation(fPerson[i].person_id, (int)floor(sim_time), length_of_stay, first_state);
      #endif
      list_file (INCREASING, first_state);
      transfer[ATTR_STATE] = (double)first_state; /* must be repeated since transfer is new */
      event_schedule(departure_day, EVENT_DEPARTURE);
    }
  }
}

/* The departure function removes the individual from the location list and moves the
   patient to a new location based on the cumulative transition probability matrix CDF */
void depart(void) {
  int state, new_state, new_new_state, worst_state;
  int days_from_diagnosis, splitting, days_in_state;
  unsigned int id;
  double departure_day, length_of_stay;
  
  state = (int)transfer[ATTR_STATE]; /* location was in EVENT_LIST's transfer */
  if (list_size[state] > 0) {
     list_remove (FIRST, state); /* now all the properties of this person is in transfer */
  }
  else {
    printf("error: you are trying to remove item from an empty list @ location %s\n", szStateVariable[state]);
    exit(1);
  }
  
  splitting = (int)transfer[ATTR_SPLITTING];
  days_from_diagnosis = (int)transfer[ATTR_DAYSDIAGNOSIS];
  days_in_state = (int) transfer[ATTR_DAYSINSTATE];
  id = (unsigned int)transfer[ATTR_PERSON];
  new_state = (int) transfer[ATTR_NEXTSTATE]; /* was decided when assigned */
  worst_state = (int) transfer[ATTR_WORSTSTATE];
  
  /* in the future we may want to route undividual patients, then tracing may be useful */
  #ifdef TRACE
  if (repeat == 1)
    printf("trace[%d]: Person-%d is leaving %s for %s on day %.2g (worst location is %s, days from diagnosis = %d)\n", repeat, id, szStateVariable[state], szStateVariable[new_state], sim_time, szStateVariable[worst_state], days_from_diagnosis);
  #endif
  if (new_state == RECOVERED) 
    numRecovered++;
  else if (new_state == DEATH)
    numDeath++;
  else {
    worst_state = MAX(new_state, worst_state); /* ICU with highest value */
    new_new_state = whereToNext(splitting, new_state, days_from_diagnosis, worst_state );
    length_of_stay = lengthOfStay(splitting, new_state, days_from_diagnosis, new_new_state);
    #ifdef TRACE
    if (repeat == 1)
      printf("[trace-x]: plan to stay here for %d days\n", (int)length_of_stay);
    #endif
    departure_day = sim_time + length_of_stay;
    transfer[ATTR_DEPARTDAY] = departure_day;
    transfer[ATTR_STATE] = (double)new_state;
    transfer[ATTR_DAYSINSTATE] = length_of_stay;
    transfer[ATTR_DAYSDIAGNOSIS] += length_of_stay;
    transfer[ATTR_WORSTSTATE] = MAX(new_state, worst_state);
    transfer[ATTR_NEXTSTATE] = (double) new_new_state;
    #ifdef STATS
    setSimulationLocation((int)id, floor(sim_time), length_of_stay, new_state);
    #endif
    list_file (INCREASING, new_state);
    transfer[ATTR_STATE] = (double)new_state; /* must be repeated since transfer is allocated again */
    event_schedule(departure_day, EVENT_DEPARTURE);
  }
}
/* report is used to write to file the current day and the length of all location lists */
void report(FILE *fid, int day, int append) {
  if (0 == append) {
    fprintf(fid,"day,home,inpatient_ward,intensive_care_unit,death,recovered\n");
  }
  else {
    fprintf(fid, "%d,", day);
    fprintf(fid, "%d,", list_size[HOME]+list_size[HOME_RED]+list_size[HOME_GREEN]);
    fprintf(fid, "%d,", list_size[INPATIENT_WARD]+list_size[INPATIENT_WARD_RED]+list_size[INPATIENT_WARD_GREEN]);
    fprintf(fid, "%d,", list_size[INTENSIVE_CARE_UNIT]+list_size[INTENSIVE_CARE_UNIT_RED]+list_size[INTENSIVE_CARE_UNIT_GREEN]);
    fprintf(fid, "%d,%d\n", numDeath, numRecovered);
   }
}

/* 
  The discrete event simulator is a simple Arrival and Departure system.
  A list is maintained for each location in order to collect statistics 
  about the location for each day and about each individual infected.
 */
int main(int argc, char *argv[]) {
 
  int listid, i, j, k, n, max_sim_time = MIN(28,MAX_SIM_TIME);
  char szDate[12], szDateLatest[12], szDateHistory[12], fname[8192];
  char path[1023], path_input[2048], path_output[2048], path_lsh_data[2048];
  char szExt[128];
  FILE *statfid;

  maxatr = 15; /* NEVER SET maxatr TO BE SMALLER THAN 4. */

  /* check user input arguments number */
  printf("\nCovid: Discrete Event Simulator (%s)\n\n", VERSION);
  if (argc < 3) {
    printf("usage: %s YYYY-MM-DD PATH_TO_DATA CA (DATA_YYYY-MM-DD HISTORY_YYYY-MM-DD MAX_SIM_TIME\n\nThe simulation starts on the YYYY-MM-DD and uses data in the PATH_TO_DATA\nThe optional DATA_YYYY-MM-DD date can be used to read data from another time period\nFinally you can set the number of days to simulate to a value less than %d (default %d)\n\n", argv[0], MAX_SIM_TIME, max_sim_time);
    printf("note: historical data (from HISTORY_YYYY-MM__DD) will be used when available for new arrivals (used for model validation),\nwhen not available new arrivals are predicted from the posterior distribution @ covid.hi.is\n\n");
    return 1;
  }
  strcpy(path, argv[2]);
  sprintf(path_input, "%s/input/", path);
  sprintf(path_lsh_data,"%s/lsh_data/", path);
  sprintf(path_output,"%s/output/", path);

  /* get the date for the simulation run */
  sprintf(szDate,"%s", argv[1]);
  sprintf(szDateLatest,"%s", argv[1]);
  sprintf(szDateHistory,"%s", argv[1]);
  if (argc > 4) { /* this is a workaround in case we want to get a newer data file and are simulating from a previous date */
    sprintf(szDateLatest,"%s", argv[4]);
    sprintf(szDateHistory,"%s", argv[4]);
  }
  if (argc > 5) 
    sprintf(szDateHistory,"%s", argv[5]);
  if (argc > 6) {
    max_sim_time = atoi(argv[6]);
    max_sim_time =  MIN(MAX_SIM_TIME,max_sim_time);
  } 
  /* by default we will use the simple extension */
  strcpy(szExt,"");
  if (argc > 3)
    if (0 == strcmp(argv[4],"CA"))
      strcpy(szExt,"_extended");

  strcat(szExt,"_1");
    
  /* get string name for all possible dates */
  get_all_dates(szDate);

  /* The file covid.out is used to report on the setting used for the run */
  sprintf(fname, "%s%s%s_covid.out", path_output, szDate, szExt);
  outfile = fopen (fname, "w");
  
  /* Generate output run file name and pointer */
  sprintf(fname, "%s%s%s_covid_simulation.csv",path_output, szDate, szExt);
  printf(" reading files from %s%s%s*.csv\n reading files from %s%s%s*.csv\n writing result in: %s\n and run data into: %s%s%s_covid.out\n\n",path_input, szDate, szExt, path_lsh_data, szDate, szExt, fname, path_output, szDate, szExt);
  statfid = fopen (fname, "w");

  /* Initialize simlib */
  init_simlib();

  /* Initialize rndlib */
  init_twister();

  /* Initialize a list for each location from HOMELOC..RECOLOC 
     The list is ranked exactly the same way as the event list INCREASING */
  for (listid = 0; listid < MAX_STATE_VARIABLE; listid++)
    list_rank[listid] = ATTR_DEPARTDAY;

  /* read the length of stay data */
  sprintf(fname, "%s%s%s_length_of_stay.csv", path_input, szDateLatest, szExt);
  readLosP(fname);

  /* load first location for new persons arrivals */
  sprintf(fname, "%s%s%s_first_state.csv", path_lsh_data, szDateLatest, szExt);
  readFirstCDF(fname);

  /* load posterior predicted values on current date and day number */
  sprintf(fname, "%s%s_iceland_posterior.csv", path_input, szDateLatest);
  fprintf(outfile,"CDFposterior[%s] = \n", szDate);
  for (i = 0; i < MAX_SIM_TIME; i++) {
    n = readHIposteriors(fname, szDate, i, CDFposterior[i]);
    fprintf(outfile,"Day-%d", i);
    for (j = 0; j < n; j++)
      fprintf(outfile,",%.4g", CDFposterior[i][j]);
    fprintf(outfile, "\n");
  }

/* load historical data about people in the Covid system */
  sprintf(fname, "%s%s%s_current_state_per_date.csv", path_lsh_data, szDateHistory, szExt);
  fprintf(outfile,"trueStates[%s] = \n", szDate);
  availTrueStateDays = readHistoricalData(fname, szDate, max_sim_time); // RJS: Do we need to read this if szDate==szDateLatest
  availHistory = availTrueStateDays;
  for (k = 0; k < numInfected; k++) {
    fprintf(outfile,"\n Person[%d]: %d\n", k, iPerson[k].person_id);
    for (j = 0; j < max_sim_time; j++)
      fprintf(outfile,"%02d ", iPerson[k].real_state[j]);
    fprintf(outfile, "\n");
  }
  fprintf(outfile, "available historical true states days for simulation from date %s is %d, taken from file %s\n", szDate, availTrueStateDays, fname);

/* load fake states too create different scenarios */
  sprintf(fname, "%s%s%s_additional_infected.csv", path_input, szDateHistory, szExt);
  availScenarios = readScenarioData(fname, szDate, max_sim_time);
  if (availScenarios > 0) {
    fprintf(outfile, "available scenarios for simulation from date %s is %d, taken from file %s\n", szDate, availScenarios, fname);
    //printPersons(fPerson, availScenarios);
  }

  /* Read the transition probability matrix */
  sprintf(fname, "%s%s%s_transition_summary.csv", path_input, szDateLatest, szExt);
  readTransitionCDF(fname);

  report(statfid,0,0); /* write header in output stats file */
  countStatistics(-1);
  if (availHistory > 0)
    printf(" available history %d days from %s\n", availHistory, szDate);
  for (repeat = 0; repeat < MAX_REPEAT; repeat++) {
    /* progress bar added for the slower machines */
    #ifndef TRACE
    printProgress((double)repeat / (MAX_REPEAT-1));
    #endif
    /* Initialize the model and fire up departure event for this in system */
    sprintf(fname, "%s%s%s_current_state.csv", path_lsh_data, szDate, szExt); // RJS this file is now read as historical data
    init_model(fname);
    //event_schedule(sim_time, EVENT_REINITIALIZE); /* used for validation purposes only */
    event_schedule(sim_time + 0.25, EVENT_ARRIVAL); /* schedule also new arrivals */
    event_schedule(sim_time + 0.75, EVENT_PRINT_STATS);
    numRecovered = 0; numDeath = 0; /* zero daily counters for this run */
    PatientId = 0; /* reset unkown patients counter */
    #ifdef STATS
    clearSimStatsLocation(max_sim_time);
    #endif
    /* Stop the simulation once the wall clock has reached MAX_SIM_TIME days */
    while ((list_size[LIST_EVENT] != 0) && (sim_time <= (double)max_sim_time)) {
    /* Determine the next event. */
      timing();
      switch (next_event_type) {
        case EVENT_REINITIALIZE:
          /* this event is for prediction validation purposes, it will reset the current state of the system based on historical data */
          //printf("re-init at %d number of person's found is %d\n", (int)floor(sim_time), numInfected);
          if (1 == real_reinit_model((int)floor(sim_time))) {
            event_schedule(sim_time + 0.25, EVENT_ARRIVAL); /* because this event was cleared */
            event_schedule(sim_time + 0.75, EVENT_PRINT_STATS);
            #ifdef TRACE
            if (repeat == 1)
              printf("trace[main-re-init] real_reinit_model @ sim_time = %g / %d the event list size is %d\n", sim_time, max_sim_time, list_size[LIST_EVENT]);
            #endif
          }
          else {
            #ifdef TRACE
            if (repeat == 1)
              printf("trace[main-re-init] failed to re-initialize with real data... at time %g (%d)\n", sim_time, (int)floor(sim_time));
            #endif
          }
          event_schedule(sim_time + 7.0, EVENT_REINITIALIZE); /* try again, perhaps no data was available befor current day */
          break;

        case EVENT_ARRIVAL:
          if (sim_time >= availHistory) {
            //printf("we should not new here?!\n");
            i = discrete_empirical(CDFposterior[(int)floor(sim_time)], MAX_INFECTED_PER_DAY, STREAM_FORECAST);
            arrive (i); /* schedule a total number of new arrivals, this value is determined by covid.hi.is model */ 
            fake_arrive((int)floor(sim_time));
            #ifdef TRACE
            if (repeat == 1)
              printf("trace[main-arrive] covid.hi.is arrivals @ sim_time = %g / %d the event list size is %d\n", sim_time, max_sim_time, list_size[LIST_EVENT]);
            #endif
          }
          else {
            real_arrive_with_id((int)floor(sim_time));
//            real_arrive((int)floor(sim_time)+1); // ATH + 1 
            #ifdef TRACE
            if (repeat == 1)
              printf("trace[main-arrive] real arrivals @ sim_time = %g / %d the event list size is %d\n", sim_time, max_sim_time, list_size[LIST_EVENT]);
            #endif 
          }
          
          event_schedule(sim_time + 1.0, EVENT_ARRIVAL); /* schedule again new arrivals end of next day */
          break;

        case EVENT_DEPARTURE:
          depart(); /* fire up the departure event */
          #ifdef TRACE
          if (repeat == 1)
            printf("trace[main-depart] depart() @ sim_time = %g / %d the event list size is %d\n", sim_time, max_sim_time, list_size[LIST_EVENT]);
          #endif
          break;

        case EVENT_PRINT_STATS:
          report(statfid,(int)floor(sim_time),1);
          numRecovered = 0; numDeath = 0; /* reset global counter for devovered and death */
          event_schedule(sim_time + 1.0, EVENT_PRINT_STATS);
          #ifdef TRACE
          if (repeat == 1)
            printf("trace[main-report] report() @ sim_time = %g / %d the event list size is %d\n", sim_time, max_sim_time, list_size[LIST_EVENT]);
          #endif
          break;

        default:
          break;
      }
    }
    #ifdef STATS
    countStatistics(max_sim_time);
    #endif
    #ifdef TRACE
      printf("correctly prediced days per location: ");
      for (k = 0; k < max_sim_time; k++) {
        for (i = 0; i < MAX_STATE_VARIABLE-2; i++)
          printf("%d, %d, %g, %g\n", k, i, countStats[k][i],countNumber[k][i]);
      }
      printf("\n");
      for (i = 0; i < max_sim_time; i++)
        printf("%02d:%02d ", iPerson[0].real_state[i], iPerson[0].simulated_state[i]);
      printf("\n");
    #endif
    reset_simlib(); /* empty all lists, ready for re-run */
  }
  /* should tidy up simlib memory here! */
  printf("\n\n");
  fclose(outfile);
  #ifdef STATS
  sprintf(fname, "%s%s_performance_check.csv", path_output, szDateHistory);
  outfile = fopen(fname, "w");
  fprintf(outfile, "day,state,correct,total\n");
  for (k = 0; k < max_sim_time; k++) {
    for (i = 0; i < MAX_STATE_VARIABLE-2; i++)
      if (countNumber[k][i] > 0)
        fprintf(outfile, "%d,%s,%.6g,%.0f\n", k, szStateVariable[i], (double)countStats[k][i] / (double)countNumber[k][i], countNumber[k][i]/MAX_REPEAT);
      else 
        fprintf(outfile, "%d,%s,1.0,%.0f\n", k, szStateVariable[i], (double)countNumber[k][i]/MAX_REPEAT);
  }
  fclose(outfile);
  sprintf(fname, "%s%s_performance_check_simulated.csv", path_output, szDateHistory);
  outfile = fopen(fname, "w");
  fprintf(outfile, "day,state,correct,total\n");
  for (k = 0; k < max_sim_time; k++) {
    for (i = 0; i < MAX_STATE_VARIABLE-2; i++)
      if (countNumberFalse[k][i] > 0)
        fprintf(outfile, "%d,%s,%.6g,%.0f\n", k, szStateVariable[i], (double)countStatsFalse[k][i] / (double)countNumberFalse[k][i], countNumberFalse[k][i]/MAX_REPEAT);
      else 
        fprintf(outfile, "%d,%s,1.0,%.0f\n", k, szStateVariable[i], (double)countNumberFalse[k][i]/MAX_REPEAT);
  }
  fclose(outfile);
  sprintf(fname, "%s%s_performance_check_simulated_mean.csv", path_output, szDateHistory);
  outfile = fopen(fname, "w");
  fprintf(outfile, "person_id,day,state,average,real,first\n");
  for (k = 0; k < numInfected; k++) {
    for (j = 0; j < max_sim_time; j++)
      for (i = 0; i < MAX_STATE_VARIABLE-2; i++) {
        fprintf(outfile, "%d,%s,%s,%.6g", iPerson[k].person_id, szAllDates[j], szStateVariable[i], iPerson[k].simulated_state_mean[i][j]/MAX_REPEAT);
        fprintf(outfile,",%d,%d\n",(iPerson[k].real_state[j] == i), (iPerson[k].first_state_indicator[j] == 1) && (iPerson[k].real_state[j] == i));
      }
  }
  fclose(outfile);
  #endif
  fclose(statfid);
  return 0;
}
