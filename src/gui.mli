(** Gui module that controls all terminal based functionality.*)

open Scheduler

(** [LocatingError] exception used to show locating error *)
exception LocatingError of string

(** [guiLocation] Type representing the location of the gui*)
type guiLocation =
  | Home
  | Create
  | SelectDay
  | ViewDay
  | ViewWeek
  | ViewPriority
  | ModifyDay 
  | Quit
  
val print_title : unit -> unit
(** [print_title] Prints the title screen of the gui*)

val print_home : unit -> unit
(** [print_home] Prints the home screen of the gui*)

val sub_2 : unit -> unit
(** [sub_2] Prints the sub_menu of the gui*)

val view_day_menu : unit -> unit
(** [view_day_menu] Prints the view day menu of the gui*)

val modify_task_menu : unit -> unit
(** [modify_task_menu] Prints the modify task menu of the gui*)

val task_to_string : task option -> int -> string
(** [task_to_string] Converts a task to a string representation*)

val schedule_to_string : schedule -> string
(** [schedule_to_string] Converts a day schedule to a string representation*)

val weekly_schedule_to_string : weekly_schedule -> day -> string
(** [weekly_schedule_to_string] converts a weekly schedule to a string
    representation*)

val print_day : weekly_schedule -> day -> unit
(** [print_day] prints a weekly schedule*)

val create_helper : weekly_schedule -> weekly_schedule
(** [create_helper] creates a new task to be added to a weekly schedule*)

val export_helper : weekly_schedule -> guiLocation
(** [export_helper] exports a weekly schedule to csv, returning a gui location *)

val home_helper : weekly_schedule -> guiLocation
(** [home_helper] Helper to manage user input of the home menu *)

val select_day_helper : weekly_schedule -> guiLocation * day option
(** [select_day_helper] Helper to manage user input of the select day menu *)

(** [view_day_helper] Helper to manage user input of the view day menu *)

val view_week_helper : weekly_schedule -> guiLocation * day option
(** [view_week_helper] Helper to manage user input of the view week menu *)

val view_priority_helper : weekly_schedule -> guiLocation * day option
(** [view_priority_helper] Helper to manage user input of the view priority
    schedule menu *)

val extract_tasks : ('a * 'b option) list -> ('a * 'b) list
(** [extract_tasks] Extracts task from task option *)

val find_task_schedule : string -> schedule -> schedule
(** [find_task_schedule] Finds a given task in a day schedule*)

val find_task_weekly_schedule :
  string -> weekly_schedule -> day -> (int * task) list option
(** [find_task_weekly_schedule] Finds a given task in a weekly schedule*)

val filter_out_day : string -> schedule -> schedule
(** [filter_out_day] Filters out a task from a day schedule*)

val filter_out_task : string -> weekly_schedule -> weekly_schedule
(** [filter_out_task] Filters out a task from a week schedule*)

(*** [modify_name_helper] Helper to modify the name of a task*)
val modify_name_helper :
  int * task -> weekly_schedule -> day -> weekly_schedule option * day

(*** [modify_start_helper] Helper to modify the start time of a task*)
val modify_start_helper :
  int * task -> weekly_schedule -> day -> weekly_schedule option * day

val modify_duration_helper :
  int * task -> weekly_schedule -> day -> weekly_schedule option * day
(** [modify_duration_helper] elper to modify the duration of a task*)

val edit_task :
  int * task -> weekly_schedule -> day -> weekly_schedule option * day
(** [edit_task] Helper to modify a task*)

val modify_day_helper : weekly_schedule -> day -> weekly_schedule option * day
(** [modify_day_helper] Helper to manage user input of modifying a day*)

val start : guiLocation -> weekly_schedule -> day option -> unit
(** [start] Main gui loop *)

val manual_task_addition : weekly_schedule -> weekly_schedule
(** [manual_task_addition] Helper to handle manual task addition*)

val smart_task_addition : weekly_schedule -> weekly_schedule
(** [smart_task_addition] Helper to handle smart task addition*)
