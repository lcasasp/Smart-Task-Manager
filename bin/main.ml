open TaskManager
open Input
open Scheduler
open Gui

let[@warning "-32"] get_input_day () =
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

let import_schedule_prompt () =
  let _ = Sys.command "Clear" in
  ();
  print_endline "Do you want to import a schedule from a JSON file? (y/n)";
  match String.lowercase_ascii (read_line ()) with
  | "y" ->
      print_endline "Enter the filename:";
      let filename = read_line () in
      import_weekly_schedule_from_json filename
  | _ -> create_empty_weekly_schedule ()

let[@warning "-32"] rec main_loop weekly_schedule =
  print_endline "Enter a command (add, export, or quit):";
  match String.lowercase_ascii (read_line ()) with
  | "add" ->
      let day = get_input_day () in
      let task = get_input_task () in
      let start_time = get_input_start_time () in
      let weekly_schedule =
        add_task_to_weekly_schedule weekly_schedule day start_time task
      in
      print_weekly_schedule weekly_schedule;
      main_loop weekly_schedule
  | "export" ->
      print_endline "Enter the filename to export:";
      let filename = read_line () in
      export_weekly_schedule_to_json filename weekly_schedule;
      print_endline "Schedule exported.";
      main_loop weekly_schedule
  | "quit" -> ()
  | _ ->
      print_endline "Invalid command.";
      main_loop weekly_schedule

let () =
  let initial_schedule = import_schedule_prompt () in
  Gui.start Home initial_schedule None
