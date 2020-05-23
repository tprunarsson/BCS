/*
  COVID-19  Population-Quarantine-Isolation-(pre)-Ward-ICU-(ventilator) 
            Discrete Event Simulator 
  author:   tpr@hi.is        
  version:  10/04/2020 (Good Friday Release)
 */

//#define TRACE
//#define DEBUG
//#define STATS
#define TRACK_PERSON
//#define TIME

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <unistd.h>

#include "simlib.h" /* Required for use of simlib.c. */
#include "rndlib.h" /* Required for use of rndlib.c. */
#include "covid.h"  /* all #DEFINEs for the this file */
#include "fileio.h" /* all reading/writing of files located here */
#include "utils.h"  /* utility function used */

int heuristics[NUM_HEURISTICS];

/* define the different splitting variable and new states */
char *szSplittingVariable[MAX_NUM_SPLITTING_VALUES];
char *szStateVariable[MAX_NUM_STATES] = {"home", "home-green", "home-red", "inpatient_ward", "inpatient_ward-green", "inpatient_ward-red", "intensive_care_unit", "intensive_care_unit-green","intensive_care_unit-red", "death", "recovered"};

/* Define the person with all of its properties in a structure */
person iPerson[MAXINFECTED];	// Infected in historical data
person sPerson[MAXINFECTED];	// Infected in scenario

person_state_time infectedAtStart[MAXINFECTED];			// Infected at the start of the simulation (from current_state file)

int numInfectedAtStart = 0;
int numInfected = 0;
int numScenarioInfected = 0;

int iPersonArrivalIndex[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
int	iPersonArrival[MAX_SIM_TIME];
int iPersonCurrentIndex[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
int iPersonCurrent[MAX_SIM_TIME];

/* CDFs */
double forecastCDF[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
double firstSplittingCDF[MAX_NUM_SPLITTING_VALUES];
double firstStateCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES];
double transitionCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_LOS_DAYS][MAX_NUM_STATES];
double losCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_LOS_DAYS];
double firstScenarioSplittingCDF[MAX_NUM_SPLITTING_VALUES];
double firstScenarioStateCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES];

/* count statistics for analysing the dynamics of the model */
double countStats[MAX_SIM_TIME+1][MAX_NUM_STATES];
double countStatsFalse[MAX_SIM_TIME+1][MAX_NUM_STATES];
double countNumber[MAX_SIM_TIME+1][MAX_NUM_STATES];
double countNumberFalse[MAX_SIM_TIME+1][MAX_NUM_STATES];

/* The real dates for each day of the simulation */ 
char szAllDates[MAX_SIM_TIME][32];

int numRecovered;
int numDeath;
int repeat;
int forecastPersonId;
int scenarioPersonId;
int numHistoryDays;

FILE *outfile; /* global file pointers for report writing */

extern char tracking_info;
extern unsigned long tracking_num_transitions;

/*
  A randomly sampled length of stay for the different states based on an empirical distribution
  defined by the CDF (see function in rndlib.c, note it returns integers 0,1,2,3 and so we add 1.0)
*/
double lengthOfStay(int splitting, state_time_data *data) {
  double length_of_stay = 0;
	int count;
	
	if (heuristics[LOS_BEFORE_TRANSITION] == 1){
		length_of_stay = discrete_empirical(losCDF[splitting][(*data).state_current], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
	} else {
		if (heuristics[RECOVER_MIN_14_DAYS] && (((*data).state_current == HOME_RED) || ((*data).state_current == HOME_GREEN) || ((*data).state_current == HOME)) && ((*data).state_next == RECOVERED)) {
			length_of_stay = discrete_empirical(losCDF[splitting][(*data).state_current], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
			while (((*data).days_from_diagnosis + length_of_stay) < 14)
				length_of_stay = (double) discrete_empirical(losCDF[splitting][(*data).state_current], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
		} else if (heuristics[HOSPITAL_LESS_THAN_14_DAYS] && (((*data).state_current == HOME_RED) || ((*data).state_current == HOME_GREEN) || ((*data).state_current == HOME)) && (((*data).state_worst == HOME_RED) || ((*data).state_worst == HOME_GREEN) || ((*data).state_worst == HOME)) && ((*data).days_from_diagnosis < 14) && ((*data).state_next != RECOVERED)){
			length_of_stay = discrete_empirical(losCDF[splitting][(*data).state_current], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
			while (((*data).days_from_diagnosis + length_of_stay) >= 14)
				length_of_stay = (double) discrete_empirical(losCDF[splitting][(*data).state_current], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
		}
		else{
			length_of_stay = discrete_empirical(losCDF[splitting][(*data).state_current], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
		}
	}
	
	// Make sure length_of_stay is not less than the days already spent in state when entering the simulation.
	// Here (total) length of stay will always be larger than the value used in the RECOVER_MIN_14_DAYS heuristic.
	count = 0;
	while (length_of_stay - (double)(*data).days_in_state < 1) {
		length_of_stay = discrete_empirical(losCDF[splitting][(*data).state_current], MAX_LOS_DAYS, STREAM_LOS) + 1.0;
		count++;
		if (count > 1000) {
			length_of_stay = (double)(*data).days_in_state + 1.0;
			break;
		}
	}
	
	(*data).length_of_stay = length_of_stay - (double)(*data).days_in_state;
	
  return (*data).length_of_stay;
}

/*
   Routing heuristic given current state and previous worst state
*/
int whereToNext(int splitting, state_time_data *data) {
  int state_next = -1;
	int length_of_stay;
	
	if (heuristics[LOS_BEFORE_TRANSITION]){
		length_of_stay = (int)(*data).length_of_stay + (*data).days_in_state; // make sure we don't forget the days before the simulation started.
	} else {
		length_of_stay = 1;	// The transition matrix is time-invariant when transitions are made before los so we can select any value between 1 and MAX_LOS_DAYS-1. We select 1.
	}
	state_next = discrete_empirical(transitionCDF[splitting][(*data).state_current][length_of_stay], MAX_NUM_STATES, STREAM_TRANSITION);
	
	if (heuristics[NO_REENTER_ICU]){
		/* this first check will only activate in case we are using the simple model */
		while ((state_next == INTENSIVE_CARE_UNIT) && ((*data).state_worst == INTENSIVE_CARE_UNIT) )
			state_next = discrete_empirical(transitionCDF[splitting][(*data).state_current][length_of_stay], MAX_NUM_STATES, STREAM_TRANSITION);

		/* this second check will only activate when we are using the clinical assessment */
		while ((state_next == INTENSIVE_CARE_UNIT_RED) && ((*data).state_worst == INTENSIVE_CARE_UNIT_RED) )
			state_next = discrete_empirical(transitionCDF[splitting][(*data).state_current][length_of_stay], MAX_NUM_STATES, STREAM_TRANSITION);
	}
	
	(*data).state_next = state_next;
	
  return state_next;
}

/*
  The init_model function reads from a file the current list of infected
  individuals in each state and puts them into a list associated.
  Note that init_simlib() will set global wall clock (sim_time) to zero.
*/
int init_model() {
  int i;
  int new_state;
  double length_of_stay, departure_day;
 
 
	for (i = 0; i < numInfectedAtStart; i++) {
		infectedAtStart[i].state_time.state_next = -1;
		infectedAtStart[i].state_time.length_of_stay = -1;
		infectedAtStart[i].state_time.days_from_symthoms = -1;
		
		if(heuristics[LOS_BEFORE_TRANSITION]){
			length_of_stay = lengthOfStay(infectedAtStart[i].splitting, &infectedAtStart[i].state_time);
			new_state = whereToNext(infectedAtStart[i].splitting, &infectedAtStart[i].state_time);
		} else {
			new_state = whereToNext(infectedAtStart[i].splitting, &infectedAtStart[i].state_time);
			length_of_stay = lengthOfStay(infectedAtStart[i].splitting, &infectedAtStart[i].state_time);
    }
		
		departure_day = floor(sim_time) + length_of_stay + 0.5;
		
		transfer[ATTR_PERSON] = (double)infectedAtStart[i].person_id;
		transfer[ATTR_SPLITTING] = (double)infectedAtStart[i].splitting;
		transfer[ATTR_DAYSINSTATE] = (double)infectedAtStart[i].state_time.days_in_state + length_of_stay;
    transfer[ATTR_DAYSDIAGNOSIS] = (double)infectedAtStart[i].state_time.days_from_diagnosis + length_of_stay;
    transfer[ATTR_STATE] = (double)infectedAtStart[i].state_time.state_current;
    transfer[ATTR_WORSTSTATE] = (double)infectedAtStart[i].state_time.state_worst;
    transfer[ATTR_NEXTSTATE] = (double)new_state;
		transfer[ATTR_DEPARTDAY] = (double)departure_day;
		
    list_file (INCREASING, infectedAtStart[i].state_time.state_current);
    transfer[ATTR_STATE] = (double)infectedAtStart[i].state_time.state_current;
    event_schedule(departure_day, EVENT_DEPARTURE);
		
		#ifdef TRACK_PERSON
			track_person(infectedAtStart[i].person_id,repeat,STATE_CURRENT,(int)floor(sim_time),infectedAtStart[i].state_time.state_current);
		#endif
  }
  return 0;
}
/*
int init_model(char *fname) {
  FILE *fid;
  int person_id, day;
  int days_from_diagnosis, days_in_state, state, new_state, splitting, worst_state, person_idx;
  double departure_day, length_of_stay;
  char buffer[1024], szstate[32], szsplitting[128], szworststate[32];
	state_time_data data;
 
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
		
		if (person_id==422531)
			data.state_current=state;
		
		data.state_current = state;
		data.state_next = -1;
		data.state_worst = worst_state;
		data.length_of_stay = -1;
		data.days_in_state = days_in_state;
		data.days_from_diagnosis = days_from_diagnosis;
		data.days_from_symthoms = -1;
		
		if(heuristics[LOS_BEFORE_TRANSITION]){
			length_of_stay = lengthOfStay(splitting, &data);
			new_state = whereToNext(splitting, &data);
		} else {
			new_state = whereToNext(splitting, &data);
			length_of_stay = lengthOfStay(splitting, &data);
    }
		
    departure_day = floor(sim_time) + length_of_stay + 0.5;
    transfer[ATTR_DAYSDIAGNOSIS] += length_of_stay;
    transfer[ATTR_NEXTSTATE] = (double)new_state;
    transfer[ATTR_DEPARTDAY] = (double)departure_day;
    iPerson[person_idx].splitting = splitting;
    iPerson[person_idx].real_days_in_state[day] = length_of_stay; // RJS probably not correct at this day. should be days_in_state?
    iPerson[person_idx].real_days_from_diagnosis[day] = days_from_diagnosis + length_of_stay; // RJS probably not correct at this day. should be date_from_diagnosis?
    iPerson[person_idx].real_state[day] = state;
    iPerson[person_idx].real_state_worst[day] = worst_state;
    #ifdef STATS
    setSimulationLocation(person_id, (int)floor(sim_time), length_of_stay, state);
    #endif
    list_file (INCREASING, state);
    transfer[ATTR_STATE] = (double)state;
    event_schedule(departure_day, EVENT_DEPARTURE);
		
		#ifdef TRACK_PERSON
			track_person(person_id,repeat,STATE_CURRENT,(int)floor(sim_time),state);
		#endif
  }
  fclose(fid);
  return 0;
}*/

/*
  The init_model function using real historical data, from given day
*/
int real_reinit_model(int day) {
  int i, k, splitting, state, days_in_state, days_from_diagnosis, new_state, worst_state, reset_model = 0;
  double length_of_stay, departure_day;
	state_time_data data;

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
			
			data.state_current = state;
			data.state_next = -1;
			data.state_worst = worst_state;
			data.length_of_stay = -1;
			data.days_in_state = days_in_state;
			data.days_from_diagnosis = days_from_diagnosis;
			data.days_from_symthoms = -1;
			
			if(heuristics[LOS_BEFORE_TRANSITION]){
				length_of_stay = lengthOfStay(splitting, &data);
				new_state = whereToNext(splitting, &data);
			} else {
				new_state = whereToNext(splitting, &data);
				length_of_stay = lengthOfStay(splitting, &data);
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
			
		#ifdef TRACK_PERSON
			track_person(iPerson[i].person_id,repeat,STATE_CURRENT,(int)floor(sim_time),state);
		#endif
    }
  }
  return reset_model;
}

/*
  The init_model function using real historical data, from given day
 NOTE: Change comment
*/
int historical_arrive(int day, int use_historical_states) {
  int i, person_index, splitting, first_state;
  int new_state;
  double length_of_stay, departure_day;
	state_time_data data;

  for (i = 0; i < iPersonArrival[day]; i++) {
		person_index = iPersonArrivalIndex[day][i];
		transfer[ATTR_PERSON] = (double)iPerson[person_index].person_id;
		splitting = iPerson[person_index].splitting;
		if (use_historical_states == 1){
			first_state = iPerson[person_index].real_state[day];
		} else {
			first_state = discrete_empirical(firstStateCDF[splitting], MAX_NUM_STATES, STREAM_FIRST_STATE);
		}
		
		data.state_current = first_state;
		data.state_next = -1;
		data.state_worst = first_state;
		data.length_of_stay = -1;
		data.days_in_state = 0;
		data.days_from_diagnosis = 0;
		data.days_from_symthoms = -1;
		
		if(heuristics[LOS_BEFORE_TRANSITION]){
			length_of_stay = lengthOfStay(splitting, &data);
			new_state = whereToNext(splitting, &data);
		} else {
			new_state = whereToNext(splitting, &data);
			length_of_stay = lengthOfStay(splitting, &data);
		}
		
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
			printf("trace[%d]: Person-%d is entering for %s on day %.2g (worst location is %s, days_from_diagnosis = %.2g)\n", repeat, (int)transfer[ATTR_PERSON], szStateVariable[location], sim_time, szStateVariable[location], transfer[ATTR_DAYSDIAG]);
		#endif
		#ifdef STATS
		setSimulationLocation(iPerson[person_index].person_id, (int)floor(sim_time), length_of_stay, state);
		#endif
		list_file (INCREASING, first_state);
		transfer[ATTR_STATE] = (double)first_state; /* must be repeated since transfer is new */
		event_schedule(departure_day, EVENT_DEPARTURE);
		
	#ifdef TRACK_PERSON
		track_person(iPerson[person_index].person_id,repeat,STATE_FIRST,(int)floor(sim_time),first_state);
	#endif
  }
  return 0;
}

/*
  The arrive function is used to generate new individuals arriving at sim_time.
  The EVENT_NEW_ARRIVE should be scheduled once every day and the total number of
  newly infected should be based on a prediction model, we use covid.hi.is
*/
void forecast_arrive(int n) {
  int i, first_state, new_state, splitting;
  double departure_day, length_of_stay;
	state_time_data data;

  for (i = 0; i < n; i++) {
    splitting=discrete_empirical(firstSplittingCDF,MAX_NUM_SPLITTING_VALUES,STREAM_SPLITTING);
    /* we need a strategy for selecting which location we enter */
    first_state = discrete_empirical(firstStateCDF[splitting], MAX_NUM_STATES, STREAM_FIRST_STATE);
		
		data.state_current = first_state;
		data.state_next = -1;
		data.state_worst = first_state;
		data.length_of_stay = -1;
		data.days_in_state = 0;
		data.days_from_diagnosis = 0;
		data.days_from_symthoms = -1;
		
		if(heuristics[LOS_BEFORE_TRANSITION]){
			length_of_stay = lengthOfStay(splitting, &data);
			new_state = whereToNext(splitting, &data);
		} else {
			new_state = whereToNext(splitting, &data);
			length_of_stay = lengthOfStay(splitting, &data);
		}
		
		transfer[ATTR_PERSON] = (double)forecastPersonId--; /* dummy ID for forecasted persons IDs */
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
		
	#ifdef TRACK_PERSON
		track_person(forecastPersonId+1,repeat,STATE_FIRST,(int)floor(sim_time),first_state);
	#endif
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
      location = discrete_empirical(firstLocCDF[agegroup], MAX_NUM_STATES, STREAM_AGE);
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
	RJS Can be simplified and perhaps combined with arrive with parameters (num arrivals and type (forecast or scenario or real)
  */
void scenario_arrive(int day) {
  int i, splitting, first_state;
  int new_state;
  double length_of_stay, departure_day;
	state_time_data data;

  for (i = 0; i < numScenarioInfected; i++) {
    if (sPerson[i].first_state_indicator[day] == 1) {
			sPerson[i].person_id = scenarioPersonId--;
			transfer[ATTR_PERSON] = (double)sPerson[i].person_id;
      splitting = discrete_empirical(firstScenarioSplittingCDF,MAX_NUM_SPLITTING_VALUES,STREAM_SPLITTING);
			first_state = discrete_empirical(firstScenarioStateCDF[splitting], MAX_NUM_STATES, STREAM_FIRST_STATE);
			
			data.state_current = first_state;
			data.state_next = -1;
			data.state_worst = first_state;
			data.length_of_stay = -1;
			data.days_in_state = 0;
			data.days_from_diagnosis = 0;
			data.days_from_symthoms = -1;
			
			if(heuristics[LOS_BEFORE_TRANSITION]){
				length_of_stay = lengthOfStay(splitting, &data);
				new_state = whereToNext(splitting, &data);
			} else {
				new_state = whereToNext(splitting, &data);
				length_of_stay = lengthOfStay(splitting, &data);
			}
			
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
        printf("trace[%d]: ScenarioPerson-%d is entering for %s on day %.2g (worst state is %s, days_from_diagnosis = %.2g)\n", repeat, (int)transfer[ATTR_PERSON], szStateVariable[first_state], sim_time, szStateVariable[first_state], transfer[ATTR_DAYSDIAG]);
      #endif
      #ifdef STATS
      setSimulationLocation(sPerson[i].person_id, (int)floor(sim_time), length_of_stay, first_state);
      #endif
      list_file (INCREASING, first_state);
      transfer[ATTR_STATE] = (double)first_state; /* must be repeated since transfer is new */
      event_schedule(departure_day, EVENT_DEPARTURE);
			
		#ifdef TRACK_PERSON
			track_person(sPerson[i].person_id,repeat,STATE_FIRST,(int)floor(sim_time),first_state);
		#endif
    }
  }
}

/* The departure function removes the individual from the location list and moves the
   patient to a new location based on the cumulative transition probability matrix CDF */
void depart(void) {
  int state, new_state, new_new_state, worst_state;
  int days_from_diagnosis, splitting, days_in_state;
  int id;
  double departure_day, length_of_stay;
	state_time_data data;
  
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
  days_in_state = (int) transfer[ATTR_DAYSINSTATE];								// RJS shouldn't this become 0?
  id = (int)transfer[ATTR_PERSON];
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
		
		if (id == 422531)
			data.state_current = new_state;
		
		data.state_current = new_state;
		data.state_next = -1;
		data.state_worst = worst_state;
		data.length_of_stay = -1;
		data.days_in_state = 0;
		data.days_from_diagnosis = days_from_diagnosis;
		data.days_from_symthoms = -1;
		
		if(heuristics[LOS_BEFORE_TRANSITION]){
			length_of_stay = lengthOfStay(splitting, &data);
			new_new_state = whereToNext(splitting, &data);
		} else {
			new_new_state = whereToNext(splitting, &data);
			length_of_stay = lengthOfStay(splitting, &data);
		}
		
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
#ifdef TRACK_PERSON
	track_person(id,repeat,STATE_OUT,(int)floor(sim_time),state);
	track_person(id,repeat,STATE_IN,(int)floor(sim_time),new_state);
		
#endif
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
 
	int listid, i, j, k;
	char fname[8192], opt;
  FILE *statfid;
	char path_input[2048], path_output[2048], path_lsh_data[2048];
	char *item;
	char date_tmp[12]="";
	int num_forecast_arrivals = 0;
	
	int	max_sim_time = MIN(28,MAX_SIM_TIME);
	int use_forecast_data = 0;
	int use_scenario_data = 0;
	int use_historical_data = 0;
	int historical_reset_days = 0;	// default is no historical reset
	int use_historical_states = 0;
	char date_start[12]="";
	char date_data[12]="";
	char date_observed[12]="";
	char experiment_id[12]="1";
	char heuristics_tmp[1024]="1;1;1;0;0";
	char splitting_values_tmp[2048]="age_0-50;age_51+";
	char path[1023]="..";

  maxatr = 15; /* NEVER SET maxatr TO BE SMALLER THAN 4. */
	
	while ((opt = getopt(argc, argv, "s:i:h:v:d:o:n:p:r:fat")) != -1) {
			switch (opt) {
			case 's': strcpy(date_start,optarg); break;
			case 'i': strcpy(experiment_id,optarg); break;
			case 'h': strcpy(heuristics_tmp,optarg); break;
			case 'v': strcpy(splitting_values_tmp,optarg); break;
			case 'd': strcpy(date_data,optarg); break;
			case 'o': strcpy(date_observed,optarg); break;
			case 'n': max_sim_time=MIN(MAX_SIM_TIME, atoi(optarg)); break;
			case 'p': strcpy(path,optarg); break;
			case 'r': use_historical_data=1;historical_reset_days=atoi(optarg); break;
			case 'f': use_forecast_data=1; break;
			case 'a': use_scenario_data=1; break;
			case 't': use_historical_states=1; break;
			default:
// RJS add help + check date formats
					fprintf(stderr, "Usage: ./%s [-simhvdnp]\n", argv[0]);
					exit(EXIT_FAILURE);
			}
	}
	
	if (strcmp(date_start,"")==0)
		exit(EXIT_FAILURE);
	else if (strcmp(date_data,"")==0)
		strcpy(date_data,date_start);
	
	if (strcmp(date_observed,"")==0){
		get_day_before(date_tmp, 12, date_data);	// the day before date_data is the last known state.
		strcpy(date_observed,date_tmp);
	}
					 
  /* check user input arguments number */
/*  printf("\nCovid: Discrete Event Simulator (%s)\n\n", VERSION);
  if (argc < 3) {
    printf("usage: %s YYYY-MM-DD PATH_TO_DATA CA (DATA_YYYY-MM-DD HISTORY_YYYY-MM-DD MAX_SIM_TIME\n\nThe simulation starts on the YYYY-MM-DD and uses data in the PATH_TO_DATA\nThe optional DATA_YYYY-MM-DD date can be used to read data from another time period\nFinally you can set the number of days to simulate to a value less than %d (default %d)\n\n", argv[0], MAX_SIM_TIME, max_sim_time);
    printf("note: historical data (from HISTORY_YYYY-MM__DD) will be used when available for new arrivals (used for model validation),\nwhen not available new arrivals are predicted from the posterior distribution @ covid.hi.is\n\n");
    return 1;
  }*/
  
	item=strtok(heuristics_tmp,";");
	i=0;
	
	while (item!=NULL) {
		heuristics[i]=atoi(item);
		i++;
		item=strtok(NULL,";");
	}
	
	item=strtok(splitting_values_tmp,";");
	i=0;
	
	while (item!=NULL) {
		szSplittingVariable[i]=item;
		i++;
		item=strtok(NULL,";");
	}
	
  sprintf(path_input, "%s/input/", path);
  sprintf(path_lsh_data,"%s/lsh_data/", path);
  sprintf(path_output,"%s/output/", path);
    
  /* get string name for all possible dates */
  get_all_dates(date_start);

  /* The file covid.out is used to report on the setting used for the run */
  sprintf(fname, "%s%s_%s_covid.out", path_output, date_start, experiment_id);
  outfile = fopen (fname, "w");
  
  /* Generate output run file name and pointer */
  sprintf(fname, "%s%s_%s_covid_simulation.csv",path_output, date_start, experiment_id);
	printf(" reading files from %s%s_%s*.csv\n reading files from %s%s_%s*.csv\n writing result in: %s\n and run data into: %s%s_%s_covid.out\n\n",path_input, date_start, experiment_id, path_lsh_data, date_start, experiment_id, fname, path_output, date_start, experiment_id);
	
#ifdef TRACK_PERSON
	tracking_num_transitions=0; // Initialize tracking
#endif
	
  statfid = fopen (fname, "w");

  /* Initialize simlib */
  init_simlib();

  /* Initialize rndlib */
  init_twister();

  /* Initialize a list for each location from HOMELOC..RECOLOC 
     The list is ranked exactly the same way as the event list INCREASING */
  for (listid = 0; listid < MAX_NUM_STATES; listid++)
    list_rank[listid] = ATTR_DEPARTDAY;

  /* read the length of stay data */
  sprintf(fname, "%s%s_%s_length_of_stay.csv", path_input, date_data, experiment_id);
  readLosCDF(fname, date_observed);

  /* load first location for new persons arrivals */
  sprintf(fname, "%s%s_%s_first_state.csv", path_input, date_data,experiment_id);
  readFirstCDF(fname, date_observed);
	
	/* Read the transition probability matrix */
	 sprintf(fname, "%s%s_%s_transition_summary.csv", path_input, date_data, experiment_id);
	 readTransitionCDF(fname, date_observed);
	
	/* Read information about infected at the start of the simulation */
	sprintf(fname, "%s%s_%s_current_state_per_date_filtered.csv", path_lsh_data, date_data, experiment_id);
	numInfectedAtStart = readInfectedAtStart(fname, date_start);
	
  /* Read the forecast from covid.hi.is */
	if (use_forecast_data == 1){
		sprintf(fname, "%s%s_infections_predicted.csv", path_input, date_data);
		readForecast(fname, date_start, max_sim_time);
	}

	numHistoryDays = 0;
	
/* load historical data about people in the Covid system if we have any  */
	if (use_historical_data==1 && 0!=strcmp(date_start,date_data)){
		sprintf(fname, "%s%s_%s_current_state_per_date.csv", path_lsh_data, date_data, experiment_id);
		fprintf(outfile,"trueStates[%s] = \n", date_start);
		
		numHistoryDays = readHistoricalData(fname, date_start, max_sim_time);
		
		for (k = 0; k < numInfected; k++) {
			fprintf(outfile,"\n Person[%d]: %d\n", k, iPerson[k].person_id);
			for (j = 0; j < max_sim_time; j++)
				fprintf(outfile,"%02d ", iPerson[k].real_state[j]);
			fprintf(outfile, "\n");
		}
		fprintf(outfile, "available historical true states days for simulation from date %s is %d, taken from file %s\n", date_start, numHistoryDays, fname);
	}
	if (numHistoryDays > 0)
		 printf(" available history %d days from %s\n", numHistoryDays, date_start);
	
	numScenarioInfected=0;
	
/* load data about scenarios */
	if (use_scenario_data==1){
		sprintf(fname, "%s%s_%s_scenario_first_state.csv", path_input, date_data, experiment_id);
		readScenarioFirstCDF(fname);
		sprintf(fname, "%s%s_%s_scenario_infected.csv", path_input, date_data, experiment_id);
		numScenarioInfected = readScenarioData(fname, date_start, max_sim_time);
		if (numScenarioInfected > 0) {
				fprintf(outfile, "available scenarios for simulation from date %s is %d, taken from file %s\n", date_start, numScenarioInfected, fname);
				//printPersons(sPerson, availScenarios);
		}
  }

  report(statfid,0,0); /* write header in output stats file */
  countStatistics(-1);
#ifdef TIME
	clock_t start=0, end=0;
#endif
	int reinit_return=0;
	
  for (repeat = 0; repeat < MAX_REPEAT; repeat++) {
    /* progress bar added for the slower machines */
    #ifndef TRACE
    printProgress((double)repeat / (MAX_REPEAT-1));
    #endif
    /* Initialize the model and fire up departure event for this in system */
    init_model();
		if (use_historical_data == 1 && historical_reset_days > 0){
			event_schedule(sim_time + historical_reset_days, EVENT_REINITIALIZE); /* used for validation purposes only */
		}
    event_schedule(sim_time + 0.25, EVENT_ARRIVAL); /* schedule also new arrivals */
    event_schedule(sim_time + 0.75, EVENT_PRINT_STATS);
    numRecovered = 0; numDeath = 0; /* zero daily counters for this run */
    forecastPersonId = FIRST_FORECAST_PERSON_ID; /* reset unkown patients counter */
		scenarioPersonId=FIRST_SCENARIO_PERSON_ID;
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
				#ifdef TIME
					if (repeat==2){
						start = clock();
					}
				#endif
					reinit_return = real_reinit_model((int)floor(sim_time));
				#ifdef TIME
					if (repeat==2){
						end = clock();
						printf("reinit_return took %f sec\n", ((double) (end - start)) / CLOCKS_PER_SEC);
					}
				#endif
					if (1 == reinit_return) {
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
          event_schedule(sim_time + historical_reset_days, EVENT_REINITIALIZE); /* try again, perhaps no data was available befor current day */
          break;

        case EVENT_ARRIVAL:
          if (sim_time >= numHistoryDays) {
						if (use_forecast_data == 1){
							num_forecast_arrivals = discrete_empirical(forecastCDF[(int)floor(sim_time)], MAX_INFECTED_PER_DAY, STREAM_FORECAST);
					#ifdef TIME
							if (repeat==1){
								start = clock();
							}
					#endif
							forecast_arrive(num_forecast_arrivals); /* schedule a total number of new arrivals, this value is determined by covid.hi.is model */
					#ifdef TIME
							if (repeat==1){
								end = clock();
								printf("%s:forecast_arrive of %d persons took %f sec\n", szAllDates[(int)floor(sim_time)], i, ((double) (end - start)) / CLOCKS_PER_SEC);
							}
					#endif
						}
						if (use_scenario_data == 1){
							scenario_arrive((int)floor(sim_time));
						}
            #ifdef TRACE
            if (repeat == 1)
              printf("trace[main-arrive] covid.hi.is arrivals @ sim_time = %g / %d the event list size is %d\n", sim_time, max_sim_time, list_size[LIST_EVENT]);
            #endif
          }
          else {
						if (use_historical_data == 1){
					#ifdef TIME
							if (repeat==1){
								start = clock();
							}
					#endif
							historical_arrive((int)floor(sim_time), use_historical_states);
					#ifdef TIME
							if (repeat==1){
								end = clock();
								printf("%s: historical arrive of %d persons took %f sec\n", szAllDates[(int)floor(sim_time)], iPersonArrival[(int)floor(sim_time)], ((double) (end - start)) / CLOCKS_PER_SEC);
							}
					#endif
						}
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
        for (i = 0; i < MAX_NUM_STATES-2; i++)
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
#ifdef TRACK_PERSON
	sprintf(fname, "%s%s_%s_tracking_info.csv",path_output, date_start, experiment_id);
	write_tracking_info(fname);
#endif
	
  #ifdef STATS
  sprintf(fname, "%s%s_performance_check.csv", path_output, szDateHistory);
  outfile = fopen(fname, "w");
  fprintf(outfile, "day,state,correct,total\n");
  for (k = 0; k < max_sim_time; k++) {
    for (i = 0; i < MAX_NUM_STATES-2; i++)
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
    for (i = 0; i < MAX_NUM_STATES-2; i++)
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
      for (i = 0; i < MAX_NUM_STATES-2; i++) {
        fprintf(outfile, "%d,%s,%s,%.6g", iPerson[k].person_id, szAllDates[j], szStateVariable[i], iPerson[k].simulated_state_mean[i][j]/MAX_REPEAT);
        fprintf(outfile,",%d,%d\n",(iPerson[k].real_state[j] == i), (iPerson[k].first_state_indicator[j] == 1) && (iPerson[k].real_state[j] == i));
      }
  }
  fclose(outfile);
  #endif
  fclose(statfid);
  return 0;
}
