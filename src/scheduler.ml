type task = {
  name : string;
  duration : int; (* in hours *)
  priority : string;
}

type day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

type schedule = (int * task option) list (* Representing each hour of the day *)
type weekly_schedule = (day * schedule) list

let string_of_day = function
  | Monday -> "Monday"
  | Tuesday -> "Tuesday"
  | Wednesday -> "Wednesday"
  | Thursday -> "Thursday"
  | Friday -> "Friday"
  | Saturday -> "Saturday"
  | Sunday -> "Sunday"

let day_of_string = function
  | "Monday" -> Monday
  | "Tuesday" -> Tuesday
  | "Wednesday" -> Wednesday
  | "Thursday" -> Thursday
  | "Friday" -> Friday
  | "Saturday" -> Saturday
  | "Sunday" -> Sunday
  | _ -> failwith "Invalid day"

(* Utility function to generate a list of all possible time slots in a day *)
let generate_schedule () =
  let rec generate_hours acc hour =
    if hour = 24 then acc else generate_hours ((hour, None) :: acc) (hour + 1)
  in
  List.rev (generate_hours [] 0)

(* Function to create an empty schedule *)
let create_empty_daily_schedule () = generate_schedule ()

let create_empty_weekly_schedule () =
  List.map
    (fun day -> (day, create_empty_daily_schedule ()))
    [ Monday; Tuesday; Wednesday; Thursday; Friday; Saturday; Sunday ]

(* Function to add a task to a schedule *)
let add_task_to_day_schedule schedule (start_time : int) task =
  let rec aux remaining_slots current_hour duration acc =
    match remaining_slots with
    | [] -> List.rev acc
    | (hour, task_option) :: rest when duration > 0 && hour = current_hour ->
        if task_option = None then
          aux rest (current_hour + 1) (duration - 1) ((hour, Some task) :: acc)
        else aux rest (current_hour + 1) duration ((hour, task_option) :: acc)
    | slot :: rest -> aux rest current_hour duration (slot :: acc)
  in
  aux schedule start_time task.duration []

let add_task_to_weekly_schedule weekly_schedule day start_time task =
  let updated_day_schedule =
    add_task_to_day_schedule (List.assoc day weekly_schedule) start_time task
  in
  List.map
    (fun (d, sch) -> if d = day then (d, updated_day_schedule) else (d, sch))
    weekly_schedule

let print_daily_schedule schedule =
  List.iter
    (fun (hour, task_option) ->
      let time_slot_str = Printf.sprintf "%02d:00 - %02d:00" hour (hour + 1) in
      match task_option with
      | Some task ->
          Printf.printf "%s: %s (Duration: %d hours)\n" time_slot_str task.name
            task.duration
      | None -> Printf.printf "%s: Free\n" time_slot_str)
    schedule

let format_daily_schedule (day : day) (schedule : schedule) =
  let header = Printf.sprintf "--- %s ---" (string_of_day day) in
  let formatted_slots =
    List.map
      (fun (hour, task_option) ->
        let time_slot_str =
          Printf.sprintf "%02d:00 - %02d:00" hour (hour + 1)
        in
        match task_option with
        | Some task ->
            Printf.sprintf "%s: %s (Duration: %d hours)" time_slot_str task.name
              task.duration
        | None -> Printf.sprintf "%s: Free" time_slot_str)
      schedule
  in
  header :: formatted_slots

let print_weekly_schedule (weekly_schedule : weekly_schedule) =
  let formatted_days =
    List.map
      (fun (day, sched) -> format_daily_schedule day sched)
      weekly_schedule
  in
  let max_length =
    List.fold_left
      (fun acc day_sched -> max acc (List.length day_sched))
      0 formatted_days
  in

  for i = 0 to max_length - 1 do
    List.iter
      (fun day_sched ->
        if i < List.length day_sched then
          Printf.printf "%-25s " (List.nth day_sched i)
        else Printf.printf "%-25s " "")
      formatted_days;
    print_newline ()
  done

let compare_priority p1 p2 =
  match (p1, p2) with
  | "High", "High" | "Medium", "Medium" | "Low", "Low" -> 0
  | "High", _ -> -1
  | _, "High" -> 1
  | "Medium", _ -> -1
  | _, "Medium" -> 1
  | _, _ -> 0

let sort_schedule_by_priority schedule =
  List.sort
    (fun (_, t1) (_, t2) ->
      match (t1, t2) with
      | Some task1, Some task2 -> compare_priority task1.priority task2.priority
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> 0)
    schedule

(* ----------------------------------------------------------- *)
(* Export/Import HELPER FUNCTIONS *)
open Yojson.Basic.Util

let task_to_json task =
  `Assoc
    [
      ("name", `String task.name);
      ("duration", `Int task.duration);
      ("priority", `String task.priority);
    ]

let time_slot_to_json (hour, task_option) =
  `Assoc
    [
      ("hour", `Int hour);
      ( "task",
        match task_option with
        | Some task -> task_to_json task
        | None -> `Null );
    ]

let daily_schedule_to_json schedule =
  `List (List.map time_slot_to_json schedule)

let weekly_schedule_to_json weekly_schedule =
  `List
    (List.map
       (fun (day, schedule) ->
         `Assoc
           [
             ("day", `String (string_of_day day));
             ("schedule", daily_schedule_to_json schedule);
           ])
       weekly_schedule)

let json_to_task json =
  {
    name = json |> member "name" |> to_string;
    duration = json |> member "duration" |> to_int;
    priority = json |> member "priority" |> to_string;
  }

let json_to_time_slot json =
  let hour = json |> member "hour" |> to_int in
  let task_json = json |> member "task" in
  let task_option =
    if task_json = `Null then None else Some (json_to_task task_json)
  in
  (hour, task_option)

let json_to_daily_schedule json = json |> to_list |> List.map json_to_time_slot

(*--------------------------------------------------------------------------------*)

(* Main function to export a weekly schedule to a JSON file *)
let export_weekly_schedule_to_json filename weekly_schedule =
  let json = weekly_schedule_to_json weekly_schedule in
  let formatted_json = Yojson.Basic.pretty_to_string json in
  let oc = open_out filename in
  Printf.fprintf oc "%s" formatted_json;
  close_out oc

let json_to_weekly_schedule json =
  json |> to_list
  |> List.map (fun json ->
         let day = json |> member "day" |> to_string |> day_of_string in
         let schedule = json |> member "schedule" |> json_to_daily_schedule in
         (day, schedule))

let import_weekly_schedule_from_json filename =
  let json = Yojson.Basic.from_file filename in
  json_to_weekly_schedule json

(*--------------------------------------------------------------------------------*)
(* Smart Scheduling *)

let is_range_free schedule start_hour duration =
  let rec aux hour count =
    if count = 0 then true
    else
      match List.assoc_opt hour schedule with
      | Some None -> aux (hour + 1) (count - 1)
      | _ -> false
  in
  aux start_hour duration

let smart_fill_day schedule day tasks preferred_start preferred_end =
  let fill_task day_schedule task =
    let rec try_fill start_hour =
      if start_hour + task.duration > 24 then day_schedule
        (* No slot available *)
      else if is_range_free day_schedule start_hour task.duration then
        add_task_to_day_schedule day_schedule start_hour task
      else try_fill (start_hour + 1)
    in
    if preferred_start <= preferred_end then try_fill preferred_start
    else try_fill 0
  in
  let day_schedule = List.assoc day schedule in
  let updated_schedule = List.fold_left fill_task day_schedule tasks in
  List.map
    (fun (d, sch) -> if d = day then (d, updated_schedule) else (d, sch))
    schedule
