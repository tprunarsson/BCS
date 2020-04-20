#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "fileio.h"
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
extern int numInfected;

extern double countStats[MAX_SIM_TIME+1][MAX_STATE_VARIABLE];
extern double countStatsFalse[MAX_SIM_TIME+1][MAX_STATE_VARIABLE];
extern double countNumber[MAX_SIM_TIME+1][MAX_STATE_VARIABLE];
extern double countNumberFalse[MAX_SIM_TIME+1][MAX_STATE_VARIABLE];

extern char szAllDates[MAX_SIM_TIME][32];

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
      for (k = 0; k < MAX_STATE_VARIABLE; k++) {
        countStats[i][k] = 0.0;
        countNumber[i][k] = 0.0;
        countStatsFalse[i][k] = 0.0;
        countNumberFalse[i][k] = 0.0;
        for (j = 0; j < numInfected; j++)
          iPerson[j].simulated_location_mean[k][i] = 0.0;
      }
    }
    return 0;
  }
  for (j = 0; j < max_sim_time; j++) {
    for (k = 0; k < numInfected; k++) {
      if (iPerson[k].real_location[j] >= 0) {
        if (iPerson[k].real_location[j] == iPerson[k].simulated_location[j])
          countStats[j][iPerson[k].real_location[j]] += 1.0;
        countNumber[j][iPerson[k].real_location[j]] += 1.0;
      }
      if (iPerson[k].simulated_location[j] >= 0) {
        if (iPerson[k].real_location[j] == iPerson[k].simulated_location[j])
          countStatsFalse[j][iPerson[k].simulated_location[j]] += 1.0;
        countNumberFalse[j][iPerson[k].simulated_location[j]] += 1.0;
      }
    }
  }
  return 1;
}

int clearSimStatsLocation(int max_sim_time) {
  int i, k;
 
  for (i = 0; i < max_sim_time; i++) {
    for (k = 0; k < numInfected; k++) {
      iPerson[k].simulated_location[i] = -1;
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
    if (iPerson[person_idx].simulated_location[i] >= 0) {
      iPerson[person_idx].simulated_location[i] = -1;
      iPerson[person_idx].simulated_location_mean[location][i] -= 1.0;
    }
    else
      break;
  }
  for (i = day; i < MIN(MAX_SIM_TIME,(day+los)); i++) {
    iPerson[person_idx].simulated_location[i] = location;
    iPerson[person_idx].simulated_location_mean[location][i] += 1.0;
  } 
  return 1;
}

void printPersons(person thePerson[MAXINFECTED], int n) {
  int i, day;

  for (i = 0; i < n; i++) {
    day = thePerson[i].start_day;
    printf("person-%d arrived at %s in %s(%d) in group %s\n", thePerson[i].person_id, thePerson[i].szDate, szStateVariable[thePerson[i].real_location[day]], thePerson[i].real_location[day], szSplittingVariable[thePerson[i].age_group]);
  }
}