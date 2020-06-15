#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "fileio.h"
#include "covid.h"

extern double CDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_NUM_STATES];
extern double losCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES][MAX_LOS_DAYS];
extern double firstLocCDF[MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES];
extern double CDFposterior[MAX_SIM_TIME][MAX_INFECTED_PER_DAY];
extern int historicalData[MAXINFECTED][MAX_NUM_SPLITTING_VALUES][MAX_NUM_STATES];

extern FILE *outfile;

extern char *szSplittingVariable[MAX_NUM_SPLITTING_VALUES];
extern char *szStateVariable[MAX_NUM_STATES];

extern person iPerson[MAXINFECTED];
extern int numInfected;

extern double countStats[MAX_SIM_TIME+1][MAX_NUM_STATES];
extern double countStatsFalse[MAX_SIM_TIME+1][MAX_NUM_STATES];
extern double countNumber[MAX_SIM_TIME+1][MAX_NUM_STATES];
extern double countNumberFalse[MAX_SIM_TIME+1][MAX_NUM_STATES];

extern char szAllDates[MAX_SIM_TIME][32];

char tracking_info[MAX_SIM_TIME*MAXINFECTED][128]; // We track all repeats
unsigned long tracking_num_transitions;

// Functions for tracking persons during the simulation. Uses global variable tracking_info and tracking_num_transitions.

void track_person(int person_id, int sim_no,char *transition, int sim_date, int state){
	sprintf(tracking_info[tracking_num_transitions],"%d,%d,%s,%s,%s",person_id, sim_no,transition,szAllDates[sim_date],szStateVariable[state]);
	tracking_num_transitions++;
}

void write_tracking_info(char *file_name){
	FILE *fp;
	unsigned long i;
	
	fp=fopen(file_name,"w");
	if(fp==NULL){
		printf("Could not open tracking info file\n");
	}
	else {
		fprintf(fp,"person_id,sim_no,transition,date,state\n");	// write header
		for (i=0;i<tracking_num_transitions;i++) {
			fprintf(fp,"%s\n",tracking_info[i]);
		}
	}
	fclose(fp);
}

size_t get_day_before(char *date_day_before, size_t max_size, char *date_day){
	struct tm  t = { 0 };
	int y,m,d;
	
	sscanf(date_day,"%d-%d-%d", &y, &m, &d);
 
	t.tm_mday = d-1;
	t.tm_mon = m - 1;
	t.tm_year = y - 1900;
	mktime(&t);
	
	return strftime(date_day_before, max_size, "%Y-%m-%d", &t);
}

size_t get_next_day(char *date_next_day, size_t max_size, char *date_day){
	struct tm  t = { 0 };
	int y,m,d;
	
	sscanf(date_day,"%d-%d-%d", &y, &m, &d);
 
	t.tm_mday = d+1;
	t.tm_mon = m - 1;
	t.tm_year = y - 1900;
	mktime(&t);
	
	return strftime(date_next_day, max_size, "%Y-%m-%d", &t);
}

int get_sim_day(char *sim_date, char *start_date){
	int year, month, day;
	struct tm  tm_date = { 0 }, tm_start_date= { 0 };
	
	sscanf(sim_date,"%d-%d-%d", &year, &month, &day);
	tm_date.tm_mday = day;
	tm_date.tm_mon = month - 1;
	tm_date.tm_year = year - 1900;

	sscanf(start_date,"%d-%d-%d", &year, &month, &day);
	tm_start_date.tm_mday = day;
	tm_start_date.tm_mon = month - 1;
	tm_start_date.tm_year = year - 1900;

	return difftime(mktime(&tm_date), mktime(&tm_start_date))/86400;
}

/* function for displaying the progress of the simulation */
void printProgress (double percentage) {
  char PBSTR[60] = "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||";
  int val = (int) (percentage * 100);
  int lpad = (int) (percentage * 60);
  int rpad = 60 - lpad;
  printf ("\r%3d%% [%.*s%*s]", val, lpad, PBSTR, rpad, "");
  fflush (stdout);
}

/* get all the true dates as string from the first day of simulation date szDate */
void get_all_dates(char szDate[32]) {
  int day;
  int y, m, d;
  struct tm  t = { 0 };

  sscanf(szDate,"%d-%d-%d", &y, &m, &d);
  for (day = 0; day < MAX_SIM_TIME; day++) {
    t.tm_mday = d;
    t.tm_mon = m - 1;
    t.tm_year = y - 1900;
    t.tm_mday += day;
    mktime(&t);
    strftime(szAllDates[day], 32, "%Y-%m-%d", &t);
  }
}

/* auxilliary file used for computing statistics for the runs */
int countStatistics(int max_sim_time) {
  int i, j, k;

  if (max_sim_time < 0) {
    for (i = 0; i < MAX_SIM_TIME+1; i++) {
      for (k = 0; k < MAX_NUM_STATES; k++) {
        countStats[i][k] = 0.0;
        countNumber[i][k] = 0.0;
        countStatsFalse[i][k] = 0.0;
        countNumberFalse[i][k] = 0.0;
        for (j = 0; j < numInfected; j++)
          iPerson[j].simulated_state_mean[k][i] = 0.0;
      }
    }
    return 0;
  }
  for (j = 0; j < max_sim_time; j++) {
    for (k = 0; k < numInfected; k++) {
      if (iPerson[k].real_state[j] >= 0) {
        if (iPerson[k].real_state[j] == iPerson[k].simulated_state[j])
          countStats[j][iPerson[k].real_state[j]] += 1.0;
        countNumber[j][iPerson[k].real_state[j]] += 1.0;
      }
      if (iPerson[k].simulated_state[j] >= 0) {
        if (iPerson[k].real_state[j] == iPerson[k].simulated_state[j])
          countStatsFalse[j][iPerson[k].simulated_state[j]] += 1.0;
        countNumberFalse[j][iPerson[k].simulated_state[j]] += 1.0;
      }
    }
  }
  return 1;
}

int clearSimStatsLocation(int max_sim_time) {
  int i, k;
 
  for (i = 0; i < max_sim_time; i++) {
    for (k = 0; k < numInfected; k++) {
      iPerson[k].simulated_state[i] = -1;
    }
  }
  return 1;
}

int setSimulationLocation(int person_id, int day, int los, int location) {
  int i, person_idx;

  person_idx = get_person_index(person_id);
  #ifdef TRACE
  if (repeat == 1)
    printf("[trace-stats] you have asked to add count of individual %d (%d) on day %d for los=%d at location %d\n", person_id, person_idx, day, los, location);
  #endif
  /* in case there is a reset from current day we need to clear the previous simulation set */
  for (i = day; i < MIN(MAX_SIM_TIME,(day+los)); i++) {
    if (iPerson[person_idx].simulated_state[i] >= 0) {
      iPerson[person_idx].simulated_state[i] = -1;
      iPerson[person_idx].simulated_state_mean[location][i] -= 1.0;
    }
    else
      break;
  }
  for (i = day; i < MIN(MAX_SIM_TIME,(day+los)); i++) {
    iPerson[person_idx].simulated_state[i] = location;
    iPerson[person_idx].simulated_state_mean[location][i] += 1.0;
  } 
  return 1;
}

void printPersons(person thePerson[MAXINFECTED], int n) {
  int i, day;

  for (i = 0; i < n; i++) {
    day = thePerson[i].start_day;
    printf("person-%d arrived at %s in %s(%d) in group %s\n", thePerson[i].person_id, thePerson[i].szDate, szStateVariable[thePerson[i].real_state[day]], thePerson[i].real_state[day], szSplittingVariable[thePerson[i].splitting]);
  }
}
