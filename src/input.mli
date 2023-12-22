(** Input module is responsible for getting input from the user. *)

open Scheduler

val get_input_duration : unit -> int
(** Prompts the user to enter a task duration *)

val get_input_task : unit -> task
(** Prompts the user to enter a task *)

val get_input_name : unit -> string
(** Prompts the user to enter a task name *)

val get_input_start_time : unit -> int
(** Prompts the user to enter a task start time *)

val get_input_day : unit -> day
(** Prompts the user to enter a day of the week *)

val get_input_priority : unit -> string
(** Prompts the user to enter a task priority *)

val get_tasks_for_smart_scheduling : unit -> task list
(** Prompts the user to enter a list of tasks for smart scheduling *)

val get_preferred_hours : unit -> int * int
(** Prompts the user to enter preferred start and end hours for tasks *)
