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

#define STREAM_LOS          1  /* Random-number stream for length of stay. */
#define STREAM_SPLITTING    2  /* Random-number stream for splitting variables. */
#define STREAM_FIRST_STATE  3  /* Random-number stream for first state. */
#define STREAM_TRANSITION   4  /* Random-number stream for state transitions. */
#define STREAM_FORECAST     5  /* Random-number stream for covid.hi.is forecasts. */

#define ATTR_SPLITTING      2 /* index in transfer for this attribute */
#define ATTR_DAYSDIAGNOSIS  3 
#define ATTR_DAYSINSTATE    4
#define ATTR_STATE          5
#define ATTR_DEPARTDAY      6
#define ATTR_PERSON         7
#define ATTR_NEXTSTATE      8
#define ATTR_WORSTSTATE     9

// "home", "home-green", "home-red", "inpatient_ward", "inpatient_ward-green", "inpatient_ward-red", "intensive_care_unit", "intensive_care_unit-green","intensive_care_unit-red", "death", "recovered"
#define HOME                            0
#define HOME_GREEN                      1
#define HOME_RED                        2
#define INPATIENT_WARD                  3
#define INPATIENT_WARD_GREEN            4
#define INPATIENT_WARD_RED              5
#define INTENSIVE_CARE_UNIT             6
#define INTENSIVE_CARE_UNIT_GREEN       7
#define INTENSIVE_CARE_UNIT_RED         8
#define DEATH                           9
#define RECOVERED                       10

#define MAX_NUM_SPLITTING_VALUES        12
#define MAX_NUM_STATES                  11

#define NO_REENTER_ICU                  0
#define RECOVER_MIN_14_DAYS             1
#define IMPUTE_RECOVERED                2

#define NUM_HEURISTICS                  3

#define COLS_FILE_FIRST_STATE                   4
#define COLS_FILE_CURRENT_STATE                 6
#define COLS_FILE_CURRENT_STATE_PER_DATE        7
#define COLS_FILE_LOS                           4
#define COLS_FILE_TRANSITIONS                   4
#define COLS_FILE_SCENARIOS											3

#define STATE_CURRENT										"state_current"
#define STATE_FIRST											"state_first"
#define STATE_IN												"state_in"
#define STATE_OUT												"state_out"

#define FIRST_PREDICTED_PERSON_ID								-1
#define FIRST_ADDITIONAL_PERSON_ID							-100001


typedef struct {
  int person_id; /* let simulated persons take negative values */
  int splitting;
  char szDate[12]; /* the date of entry */
  int start_day;
  int real_state[MAX_SIM_TIME]; /* use -1 when not in system */
  int real_state_worst[MAX_SIM_TIME]; /* use -1 when not in system */
  int simulated_state[MAX_SIM_TIME];
  double simulated_state_mean[RECOVERED-HOME+1][MAX_SIM_TIME];
  int simulated_state_worst[MAX_SIM_TIME];
  int real_days_in_state[MAX_SIM_TIME];
  int simulated_days_in_state[MAX_SIM_TIME];
  int real_days_from_diagnosis[MAX_SIM_TIME];
  int simulated_days_from_diagnosis[MAX_SIM_TIME];
  int first_state_indicator[MAX_SIM_TIME];
} person;

