void track_person(int person_id, int sim_no,char *transition, int sim_date, int state);
void write_tracking_info(char *file_name);
size_t get_next_day(char *date_next_day,size_t max_size, char *date_day);
int get_sim_day(char *date, char *start_date);
void printProgress (double percentage);
void get_all_dates(char szDate[32]);
int clearSimStatsLocation(int max_sim_time);
int setSimulationLocation(int person_id, int day, int los, int location);
int countStatistics(int max_sim_time);
void printPersons(person thePerson[MAXINFECTED], int n);
int use_heuristic(int heuristic);

