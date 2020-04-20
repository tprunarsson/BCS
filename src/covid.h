#define VERSION     "10-April-2020 Good Friday"

#define MAXINFECTED 65536 /* estimated MAX value infected! */
#define MAX_INFECTED_PER_DAY 8192 /* estimated MAX value infected! */
#define MAX_REPEAT 1000 /* number of times we replicated the simulation */

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#define MAX_LOS_DAYS        60
#define MAX_SIM_TIME        84

#define EVENT_ARRIVAL       10
#define EVENT_DEPARTURE     12
#define EVENT_REINITIALIZE  13
#define EVENT_SCENARIO      14
#define EVENT_PRINT_STATS   15

#define STREAM_LOS          1  /* Random-number stream for interarrivals. */
#define STREAM_AGE          2  /* Random-number stream for service times. */

#define ATTR_AGEGROUP       2 /* index in transfer for this attribute */
#define ATTR_DAYSDIAG       3 
#define ATTR_DAYSINLOC      4
#define ATTR_LOCATION       5
#define ATTR_DEPARTDAY      6
#define ATTR_PERSON         7
#define ATTR_NEXTLOCATION   8
#define ATTR_LASTWORSTLOC   9

// "home", "home-green", "home-red", "inpatient_ward", "inpatient_ward-green", "inpatient_ward-red", "intensive_care_unit", "intensive_care_unit-green","intensive_care_unit-red", "death", "recovered"
#define HOME                      0
#define HOME_GREEN                1
#define HOME_RED                  2
#define INPATIENT_WARD            3
#define INPATIENT_WARD_GREEN      4
#define INPATIENT_WARD_RED        5
#define INTENSIVE_CARE_UNIT       6
#define INTENSIVE_CARE_UNIT_GREEN 7
#define INTENSIVE_CARE_UNIT_RED   8
#define DEATH                     9
#define RECOVERED                10

#define MAX_SPLITTING_VARIABLE    2
#define MAX_STATE_VARIABLE       11

typedef struct {
  int person_id; /* let simulated persons take negative values */
  int age_group;
  char szDate[12]; /* the date of entry */
  int start_day;
  int real_location[MAX_SIM_TIME]; /* use -1 when not in system */
  int real_location_worst[MAX_SIM_TIME]; /* use -1 when not in system */
  int simulated_location[MAX_SIM_TIME];
  double simulated_location_mean[RECOVERED-HOME+1][MAX_SIM_TIME];
  int simulated_location_worst[MAX_SIM_TIME];
  int real_days_in_location[MAX_SIM_TIME];
  int simulated_days_in_location[MAX_SIM_TIME];
  int real_days_from_diagnosis[MAX_SIM_TIME];
  int simulated_days_from_diagnosis[MAX_SIM_TIME];
  int first_state_indicator[MAX_SIM_TIME];
} person;

