void printProgress (double percentage);
void get_all_dates(char szDate[32]);
int clearSimStatsLocation(int max_sim_time);
int setSimulationLocation(int person_id, int day, int los, int location);
int countStatistics(int max_sim_time);
void printPersons(person thePerson[MAXINFECTED], int n);