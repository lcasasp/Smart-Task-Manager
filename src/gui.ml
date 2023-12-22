open Scheduler
open Input

exception LocatingError of string

type guiLocation =
  | Home
  | Create
  | SelectDay
  | ViewDay
  | ViewWeek
  | ViewPriority
  | ModifyDay
  | Quit

let print_title () =
  print_endline
    "  _____ __    __  _  __   __   ____  _ ___ __  _  _ _   ___ ___  \n\
    \  |_   _/   /' _/| |/ / /' _/ / _/ || | __| _\\| || | | | __| _  \n\
    \    | || / |`._`.|   <  `._`.| \\_| >< | _|| v | \\/ | |_| _|| v / \n\
    \    |_||_||_||___/|_|\\_ |___/ \\__/_||_|___|__/ \\__/|___|___|_|_ "

let print_home () =
  print_title ();
  print_endline
    "\n\
    \  1 - Input a new task \n\
    \  2 - Print current schedule for a specific day\n\
    \  3 - Print entire weekly schedule\n\
    \  4 - Print schedule sorted by priority\n\
    \  5 - Export schedule to JSON file\n\
    \  q - Quit\n\
    \  "

let sub_2 () =
  print_title ();
  print_endline
    "\n\
    \  Select a day of week to view:\n\
    \  Monday - 1\n\
    \  Tuesday - 2\n\
    \  Wednesday - 3\n\
    \  Thursday - 4\n\
    \  Friday - 5\n\
    \  Saturday - 6\n\
    \  Sunday - 7\n\
    \  go back - r\n\
    \  "

let view_day_menu () = print_endline "\n  Go home - r\n  Modify Task - 1\n  "
let view_week_menu () = print_endline "Go home - r"

let modify_task_menu () =
  print_endline
    "\n\
    \  Which part of the task would you like to modify:\n\
    \  1 - Task Name\n\
    \  2 - Task Start Time\n\
    \  3 - Task Duration\n\
    \  4 - Task Priority\n\
    \  r - Go Back\n\
    \  "

let task_to_string (x : task option) (hour : int) =
  match x with
  | Some t ->
      " Task: " ^ t.name ^ "\n" ^ " Start: " ^ string_of_int hour ^ "\n"
      ^ " End: "
      ^ string_of_int (hour + 1)
      ^ "\n" ^ "------------------------ \n"
  | None ->
      "Free Block - " ^ string_of_int hour ^ "\n" ^ "------------------------\n"

let rec schedule_to_string (s : schedule) =
  match s with
  | (hour, task) :: t -> task_to_string task hour ^ schedule_to_string t
  | [] -> ""

let rec weekly_schedule_to_string (s : weekly_schedule) (d : day) =
  let _ = Sys.command "Clear" in
  ();
  match s with
  | [] -> ""
  | (day, sched) :: t ->
      if day = d then
        "--------" ^ string_of_day day ^ "---------- \n"
        ^ schedule_to_string sched
      else weekly_schedule_to_string t d

let print_day (s : weekly_schedule) (d : day) =
  print_string (weekly_schedule_to_string s d)

let print_sorted_schedule (schedule : weekly_schedule) day =
  Printf.printf "--- %s ---\n" (string_of_day day);
  let day_schedule = List.assoc day schedule in
  let sorted_sched = Scheduler.sort_schedule_by_priority day_schedule in
  print_daily_schedule sorted_sched;
  print_newline ()

let rec post_schedule_view () =
  print_endline "Select an option:";
  print_endline "r - Return to home";
  match String.lowercase_ascii (read_line ()) with
  | "r" -> ()
  | _ -> post_schedule_view ()

let rec print_sorted_day_schedule (schedule : weekly_schedule) =
  sub_2 ();
  match String.lowercase_ascii (read_line ()) with
  | "1" ->
      print_sorted_schedule schedule Monday;
      post_schedule_view ()
  | "2" ->
      print_sorted_schedule schedule Tuesday;
      post_schedule_view ()
  | "3" ->
      print_sorted_schedule schedule Wednesday;
      post_schedule_view ()
  | "4" ->
      print_sorted_schedule schedule Thursday;
      post_schedule_view ()
  | "5" ->
      print_sorted_schedule schedule Friday;
      post_schedule_view ()
  | "6" ->
      print_sorted_schedule schedule Saturday;
      post_schedule_view ()
  | "7" ->
      print_sorted_schedule schedule Sunday;
      post_schedule_view ()
  | "r" -> ()
  | _ ->
      print_endline "Invalid Input. Reselect please.";
      print_sorted_day_schedule schedule

let manual_task_addition schedule =
  let day = get_input_day () in
  let task = get_input_task () in
  let start_time = get_input_start_time () in
  add_task_to_weekly_schedule schedule day start_time task

let smart_task_addition schedule =
  let day = get_input_day () in
  let preferred_start, preferred_end = get_preferred_hours () in
  let tasks = get_tasks_for_smart_scheduling () in
  smart_fill_day schedule day tasks preferred_start preferred_end

let rec create_helper (schedule : weekly_schedule) =
  print_endline "Choose task scheduling method: 1 for Manual, 2 for Smart:";
  match read_line () with
  | "1" -> manual_task_addition schedule
  | "2" -> smart_task_addition schedule
  | _ ->
      print_endline "Invalid option. Please choose again.";
      create_helper schedule

let export_helper (schedule : weekly_schedule) =
  print_endline "Enter the filename to export:";
  let filename = read_line () in
  export_weekly_schedule_to_json filename schedule;
  print_endline "Schedule exported.";
  Home

let home_helper (schedule : weekly_schedule) =
  print_home ();
  match String.lowercase_ascii (read_line ()) with
  | "1" -> Create
  | "2" -> SelectDay
  | "3" -> ViewWeek
  | "4" -> ViewPriority
  | "5" -> export_helper schedule
  | "q" -> Quit
  | _ ->
      print_endline "Invalid Input. Reselect please.";
      Home

let rec select_day_helper (schedule : weekly_schedule) =
  sub_2 ();
  match String.lowercase_ascii (read_line ()) with
  | "1" -> (ViewDay, Some Monday)
  | "2" -> (ViewDay, Some Tuesday)
  | "3" -> (ViewDay, Some Wednesday)
  | "4" -> (ViewDay, Some Thursday)
  | "5" -> (ViewDay, Some Friday)
  | "6" -> (ViewDay, Some Saturday)
  | "7" -> (ViewDay, Some Sunday)
  | "r" -> (Home, None)
  | _ ->
      print_endline "Invalid Input. Reselect please.";
      select_day_helper schedule

let rec view_day_helper (schedule : weekly_schedule) (day : day option) =
  match day with
  | None -> (Home, None)
  | Some x -> (
      print_day schedule x;
      view_day_menu ();
      match String.lowercase_ascii (read_line ()) with
      | "1" -> (ModifyDay, Some x)
      | "r" -> (Home, None)
      | _ ->
          print_endline "Invalid Input. Reselect please.";
          view_day_helper schedule day)

let rec view_week_helper (schedule : weekly_schedule) =
  print_weekly_schedule schedule;
  view_week_menu ();
  match String.lowercase_ascii (read_line ()) with
  | "r" -> (Home, None)
  | _ ->
      print_endline "Invalid Input. Reselect please.";
      view_week_helper schedule

let rec view_priority_helper (schedule : weekly_schedule) =
  print_sorted_day_schedule schedule;
  view_week_menu ();
  match String.lowercase_ascii (read_line ()) with
  | "r" -> (Home, None)
  | _ ->
      print_endline "Invalid Input. Reselect please.";
      view_priority_helper schedule

let rec extract_tasks entries =
  match entries with
  | [] -> []
  | (x, Some y) :: t -> (x, y) :: extract_tasks t
  | (_, None) :: t -> extract_tasks t

let find_task_schedule (x : string) (schedule : schedule) =
  List.filter
    (fun (_, task) ->
      match task with
      | Some t -> if t.name = x then true else false
      | None -> false)
    schedule

let find_task_weekly_schedule (x : string) (schedule : weekly_schedule)
    (day : day) =
  let located =
    List.fold_left
      (fun acc (d, sub_schedule) ->
        if d <> day then acc
        else (day, find_task_schedule x sub_schedule) :: acc)
      [] schedule
  in
  match located with
  | (_, entries) :: _ -> Some (extract_tasks entries)
  | _ -> None

let filter_out_day (x : string) (schedule : schedule) =
  List.fold_right
    (fun (time, task) acc ->
      match task with
      | Some t ->
          if t.name = x then (time, None) :: acc else (time, task) :: acc
      | None -> (time, task) :: acc)
    schedule []

let filter_out_task (x : string) (schedule : weekly_schedule) : weekly_schedule
    =
  let l =
    List.fold_left
      (fun acc (day, sub_schedule) ->
        (day, filter_out_day x sub_schedule) :: acc)
      [] schedule
  in
  List.rev_append l []

let modify_name_helper entry schedule day =
  let new_name = get_input_name () in
  let start, task = entry in
  let modified_task =
    { name = new_name; duration = task.duration; priority = task.priority }
  in
  let filtered_schedule = filter_out_task task.name schedule in
  let modified_schedule =
    add_task_to_weekly_schedule filtered_schedule day start modified_task
  in
  let _ = Sys.command "exit" in
  ();
  (Some modified_schedule, day)

let modify_start_helper entry schedule day =
  let new_start = get_input_start_time () in
  let _, task = entry in
  let filtered_schedule = filter_out_task task.name schedule in
  let modified_schedule =
    add_task_to_weekly_schedule filtered_schedule day new_start task
  in
  let _ = Sys.command "exit" in
  ();
  (Some modified_schedule, day)

let modify_duration_helper entry schedule day =
  let new_duration = get_input_duration () in
  let start, task = entry in
  let modified_task =
    { name = task.name; duration = new_duration; priority = task.priority }
  in
  let filtered_schedule = filter_out_task task.name schedule in
  let modified_schedule =
    add_task_to_weekly_schedule filtered_schedule day start modified_task
  in
  let _ = Sys.command "exit" in
  ();
  (Some modified_schedule, day)

let modify_priority_helper entry schedule day =
  print_endline "Enter the new priority (High, Medium, Low):";
  let new_priority = get_input_priority () in
  (* Assuming get_input_priority fetches priority from user *)
  let start, task = entry in
  let modified_task =
    { name = task.name; duration = task.duration; priority = new_priority }
  in
  let filtered_schedule = filter_out_task task.name schedule in
  let modified_schedule =
    add_task_to_weekly_schedule filtered_schedule day start modified_task
  in
  (Some modified_schedule, day)

let rec edit_task (entry : int * task) (schedule : weekly_schedule) (day : day)
    =
  modify_task_menu ();
  match String.lowercase_ascii (read_line ()) with
  | "1" -> modify_name_helper entry schedule day
  | "2" -> modify_start_helper entry schedule day
  | "3" -> modify_duration_helper entry schedule day
  | "4" -> modify_priority_helper entry schedule day
  | "r" -> (None, day)
  | _ ->
      print_endline "Invalid Input. Reselect please.";
      edit_task entry schedule day

let rec modify_day_helper (schedule : weekly_schedule) (day : day) =
  print_endline "Which task would you like to modify?";
  match String.lowercase_ascii (read_line ()) with
  | x -> (
      match find_task_weekly_schedule x schedule day with
      | Some x ->
          let time, sample_task = List.hd x in
          edit_task (time, sample_task) schedule day
      | None ->
          let _ =
            print_endline "Unable to find task with that name. Try again."
          in
          ();
          modify_day_helper schedule day)

let rec start (loc : guiLocation) (schedule : weekly_schedule)
    (day : day option) =
  match loc with
  | Home ->
      let _ = Sys.command "Clear" in
      ();
      start (home_helper schedule) schedule None
  | Create ->
      let _ = Sys.command "Clear" in
      ();
      let updated_schedule = create_helper schedule in
      start Home updated_schedule None
  | SelectDay ->
      let _ = Sys.command "Clear" in
      ();
      let loc, d = select_day_helper schedule in
      start loc schedule d
  | ViewDay ->
      let loc, d = view_day_helper schedule day in
      start loc schedule d
  | ViewWeek ->
      let _ = Sys.command "Clear" in
      ();
      let loc, day = view_week_helper schedule in
      start loc schedule day
  | ViewPriority ->
      let _ = Sys.command "Clear" in
      let loc, day = view_week_helper schedule in
      start loc schedule day
  | Quit ->
      let _ = Sys.command "exit" in
      ()
  | ModifyDay -> (
      match day with
      | None -> start Home schedule None
      | Some x -> (
          let new_schedule = modify_day_helper schedule x in
          match new_schedule with
          | Some x, d -> start ViewDay x (Some d)
          | None, d -> start ViewDay schedule (Some d)))
