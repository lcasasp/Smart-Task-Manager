open Scheduler

let get_input_duration () =
  print_endline "Enter task duration (in hours):";
  int_of_string (read_line ())

let get_input_name () =
  print_endline "Enter task name:";
  read_line ()

let get_input_priority () =
  print_endline "Enter task priority (High, Medium, Low):";
  read_line ()

let get_input_task () =
  let name = get_input_name () in
  let duration = get_input_duration () in
  let priority = get_input_priority () in
  { name; duration; priority }

let get_input_start_time () =
  print_endline "Enter start time (in 24-hour format, e.g., 13 for 1 PM):";
  int_of_string (read_line ())

let rec get_input_day () =
  print_endline "Choose a day (Mon, Tue, Wed, Thu, Fri, Sat, Sun):";
  match String.lowercase_ascii (read_line ()) with
  | "mon" -> Monday
  | "tue" -> Tuesday
  | "wed" -> Wednesday
  | "thu" -> Thursday
  | "fri" -> Friday
  | "sat" -> Saturday
  | "sun" -> Sunday
  | _ ->
      print_endline "Invalid input. Please enter a valid day.";
      get_input_day ()

let get_preferred_hours () =
  print_endline "Do you want to specify preferred hours? (y/n):";
  if String.lowercase_ascii (read_line ()) = "y" then (
    print_endline "Enter preferred start hour (8 for 8 AM):";
    let start_hour = int_of_string (read_line ()) in
    print_endline "Enter preferred end hour (22 for 10 PM):";
    let end_hour = int_of_string (read_line ()) in
    (start_hour, end_hour))
  else (8, 22)
(* Default to 8 AM to 10 PM *)

let rec get_tasks_for_smart_scheduling () =
  print_endline "Enter task name (or 'done' to finish):";
  let name = read_line () in
  if name = "done" then []
  else
    let duration = get_input_duration () in
    let priority = get_input_priority () in
    { name; duration; priority } :: get_tasks_for_smart_scheduling ()
