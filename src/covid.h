#define VERSION     "1-April-2020"

#define MAXINFECTED 65536 /* estimated MAX value infected! */
#define MAX_INFECTED_PER_DAY 8192 /* estimated MAX value infected! */

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#define MAX_AGE_GROUPS       2
#define MAX_LOS_DAYS        60
#define MAX_SIM_TIME        28

#define EVENT_ARRIVAL        1
#define EVENT_DEPARTURE      2

#define STREAM_LOS          1  /* Random-number stream for interarrivals. */
#define STREAM_AGE          2  /* Random-number stream for service times. */

#define ATTR_AGEGROUP       2 /* index in transfer for this attribute */
#define ATTR_DAYS           3 
#define ATTR_GENDER         4
#define ATTR_DAYSINLOC      5
#define ATTR_LOCATION       6
#define ATTR_DEPARTDAY      7
#define ATTR_PERSON         8

// home,emergency_room,outpatient_clinic,inpatient_ward,intensive_care_unit,death,recovered

#define HOME                0
#define INPATIENT_WARD      1
#define INTENSIVE_CARE_UNIT 2
#define DEATH               3
#define RECOVERED           4
