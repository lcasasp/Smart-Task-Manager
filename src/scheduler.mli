(** Scheduler module which handles functionality for task management and daily/weekly scheduling*)

(** [task] is a record type which contains the name, duration, and priority of a task*)
type task = {
  name : string;
  duration : int;
  priority : string;
}

(** [day] is a type which represents the days of the week*)
type day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

(** [schedule] is a list of tuples which contains the time of day and the task
    scheduled for that time*)
type schedule = (int * task option) list
  
(** [weekly_schedule] is a list of tuples which contains the day of the week
    and the schedule for that day*)
type weekly_schedule = (day * schedule) list

val string_of_day : day -> string
(** Converts a day to a string *)

val day_of_string : string -> day
(** Converts a string to a day *)

val generate_schedule : unit -> schedule
(** Generates an empty daily schedule *)

val create_empty_daily_schedule : unit -> schedule
(** Creates an empty daily schedule *)

val create_empty_weekly_schedule : unit -> weekly_schedule
(** Creates an empty weekly schedule *)

val add_task_to_day_schedule : schedule -> int -> task -> schedule
(** Adds a task to a daily schedule at a specific hour *)

val add_task_to_weekly_schedule :
  weekly_schedule -> day -> int -> task -> weekly_schedule
(** Adds a task to a weekly schedule on a specific day at a specific hour *)

val sort_schedule_by_priority : schedule -> schedule
(** Sorts a daily schedule by task priority *)

val print_daily_schedule : schedule -> unit
(** Prints a daily schedule *)

val print_weekly_schedule : weekly_schedule -> unit
(** Prints a weekly schedule *)

val export_weekly_schedule_to_json : string -> weekly_schedule -> unit
(** Exports a weekly schedule to a JSON file *)

val import_weekly_schedule_from_json : string -> weekly_schedule
(** Imports a weekly schedule from a JSON file *)

val smart_fill_day :
  weekly_schedule -> day -> task list -> int -> int -> weekly_schedule
(** Fills a day in a weekly schedule with tasks, starting and ending at specific hours *)
