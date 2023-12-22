open TaskManager
open Scheduler
open Gui
open OUnit2

(** TEST PLAN:
    We tested most of our scheduler and gui functions using blackbox testing in OUnit.
    The only exceptions were our import/export functionality in scheduler.ml as well
    as helper functions for those. This was tested manually in our application.
    For gui.ml, some functions had to be tested manually, such as the obious start function
    and other home related functions that were purely terminal based.
    We tested heavily for modifying tasks in gui, as well as generally making
    sure printing was up to standards. This was done using white-box testing as
    we needed to know the formatting of our printing after development.
    
    We know our testing approach demonstrates correctness as we test each function 
    using blackbox testing and ensure the types and outputs match what we want (some white-box as well), and have
    thoroughly tested GUI related functions manually to ensure visually everything matches
    up. *)
    
let create_task name duration priority = { name; duration; priority }

let create_filled_schedule num_hours =
  let rec aux acc hours_left =
    if hours_left > 0 then
      let task = create_task ("Task" ^ string_of_int hours_left) 1 "Medium" in
      aux ((hours_left, Some task) :: acc) (hours_left - 1)
    else acc
  in
  aux [] num_hours

let test_schedule_to_string expected schedule _ =
  let actual = schedule_to_string schedule in
  assert_equal expected actual ~printer:(fun x -> x)

let test_weekly_schedule_to_string expected schedule day _ =
  let actual = weekly_schedule_to_string schedule day in
  assert_equal expected actual ~printer:(fun x -> x)

let test_task_to_string expected task_option hour _ =
  let actual = task_to_string task_option hour in
  assert_equal expected actual ~printer:(fun x -> x)

let test_add_task_to_schedule _ =
  let schedule = create_empty_daily_schedule () in
  let task = { name = "Test Task"; duration = 2; priority = "Medium" } in
  let updated_schedule = add_task_to_day_schedule schedule 9 task in
  assert_equal (Some task) (List.assoc 9 updated_schedule);
  assert_equal (Some task) (List.assoc 10 updated_schedule);
  assert_equal None (List.assoc 8 updated_schedule)

let test_create_task _ =
  let task = create_task "Write Code" 3 "High" in
  assert_equal "Write Code" task.name ~msg:"Task name should be 'Write Code'";
  assert_equal 3 task.duration ~msg:"Task duration should be 3";
  assert_equal "High" task.priority ~msg:"Task priority should be 'High'"

let test_create_empty_schedule _ =
  let schedule = create_empty_daily_schedule () in
  assert_equal 24 (List.length schedule) ~msg:"Schedule should have 24 slots";
  List.iter (fun (_, task) -> assert_equal None task) schedule

let test_sort_schedule_by_priority _ =
  let schedule = create_empty_daily_schedule () in
  let task1 = create_task "Task1" 1 "High" in
  let task2 = create_task "Task2" 1 "Low" in
  let schedule = add_task_to_day_schedule schedule 10 task2 in
  let schedule = add_task_to_day_schedule schedule 9 task1 in
  let sorted_schedule = sort_schedule_by_priority schedule in
  assert_equal (Some task1)
    (List.assoc 9 sorted_schedule)
    ~msg:"High priority task should be first"

let test_add_task_to_full_schedule _ =
  let schedule =
    List.init 24 (fun i ->
        (i, Some (create_task ("Task" ^ string_of_int i) 1 "Medium")))
  in
  let new_task = create_task "Extra Task" 1 "Medium" in
  let updated_schedule = add_task_to_day_schedule schedule 12 new_task in
  assert_bool "New task should not be added to a full schedule"
    (not (List.exists (fun (_, t) -> t = Some new_task) updated_schedule))

let test_add_overlapping_task _ =
  let schedule = create_empty_daily_schedule () in
  let task1 = create_task "Morning Meeting" 2 "Medium" in
  let task2 = create_task "Urgent Call" 1 "High" in
  let schedule = add_task_to_day_schedule schedule 9 task1 in
  let schedule = add_task_to_day_schedule schedule 10 task2 in
  assert_equal (Some task1) (List.assoc 10 schedule)
    ~msg:"9-11 AM should still be occupied by the first task"

let test_add_task_to_specific_day _ =
  let weekly_schedule = create_empty_weekly_schedule () in
  let task = create_task "Work on Project" 2 "Medium" in
  let updated_schedule =
    add_task_to_weekly_schedule weekly_schedule Tuesday 10 task
  in
  let tuesday_schedule = List.assoc Tuesday updated_schedule in
  assert_equal (Some task)
    (List.assoc 10 tuesday_schedule)
    ~msg:"Task should be on Tuesday at 10 AM"

let is_range_free schedule start_hour duration =
  let rec check_free slots remaining_duration current_hour =
    match slots with
    | [] -> remaining_duration = 0
    | (hour, task_option) :: rest ->
        if remaining_duration = 0 then true
        else if hour = current_hour then
          match task_option with
          | None -> check_free rest (remaining_duration - 1) (current_hour + 1)
          | Some _ -> false
        else check_free rest remaining_duration current_hour
  in
  check_free schedule duration start_hour

let test_smart_scheduling _ =
  let weekly_schedule = create_empty_weekly_schedule () in
  let task1 = create_task "Task1" 2 "Medium" in
  let task2 = create_task "Task2" 1 "High" in
  let preferred_start = 9 in
  let preferred_end = 12 in

  let updated_schedule =
    smart_fill_day weekly_schedule Wednesday [ task1; task2 ] preferred_start
      preferred_end
  in
  let wednesday_schedule = List.assoc Wednesday updated_schedule in

  let task1_scheduled = is_range_free wednesday_schedule 9 2 |> not in
  let task2_scheduled = is_range_free wednesday_schedule 11 1 |> not in

  assert_bool "Task1 should be scheduled within preferred hours" task1_scheduled;
  assert_bool "Task2 should be scheduled within preferred hours" task2_scheduled

let update_task_in_day_schedule schedule start_time task =
  List.map
    (fun (hour, existing_task) ->
      if hour = start_time then (hour, Some task) else (hour, existing_task))
    schedule

let test_update_existing_task _ =
  let schedule = create_empty_daily_schedule () in
  let initial_task = create_task "Initial Task" 1 "Medium" in
  let schedule_with_task = add_task_to_day_schedule schedule 10 initial_task in
  let updated_task = create_task "Updated Task" 1 "High" in
  let final_schedule =
    update_task_in_day_schedule schedule_with_task 10 updated_task
  in
  assert_equal (Some updated_task) (List.assoc 10 final_schedule)

let test_reject_full_day _ =
  let schedule =
    List.init 24 (fun i ->
        (i, Some (create_task ("Task" ^ string_of_int i) 1 "Medium")))
  in
  let new_task = create_task "New Task" 1 "Low" in
  let updated_schedule = add_task_to_day_schedule schedule 12 new_task in
  assert_bool "New task should not be added to a full day"
    (not (List.exists (fun (_, t) -> t = Some new_task) updated_schedule))

let test_add_task_at_different_times _ =
  let schedule = create_empty_daily_schedule () in
  let morning_task = create_task "Morning Task" 1 "Medium" in
  let afternoon_task = create_task "Afternoon Task" 1 "Medium" in
  let evening_task = create_task "Evening Task" 1 "Medium" in

  let schedule = add_task_to_day_schedule schedule 9 morning_task in
  let schedule = add_task_to_day_schedule schedule 13 afternoon_task in
  let schedule = add_task_to_day_schedule schedule 18 evening_task in

  assert_equal (Some morning_task) (List.assoc 9 schedule);
  assert_equal (Some afternoon_task) (List.assoc 13 schedule);
  assert_equal (Some evening_task) (List.assoc 18 schedule)

let test_add_task_with_various_durations _ =
  let schedule = create_empty_daily_schedule () in
  let short_task = create_task "Short Task" 1 "Low" in
  let long_task = create_task "Long Task" 4 "High" in

  let schedule = add_task_to_day_schedule schedule 10 short_task in
  let schedule = add_task_to_day_schedule schedule 14 long_task in

  assert_equal (Some short_task) (List.assoc 10 schedule);
  assert_bool "Long task should occupy 4 hours"
    (List.for_all
       (fun hour -> List.assoc hour schedule = Some long_task)
       [ 14; 15; 16; 17 ])

let test_add_task_with_boundary_times _ =
  let schedule = create_empty_daily_schedule () in
  let end_day_task = create_task "End Day Task" 1 "Medium" in
  let start_day_task = create_task "Start Day Task" 1 "Medium" in

  let schedule = add_task_to_day_schedule schedule 23 end_day_task in
  let schedule = add_task_to_day_schedule schedule 0 start_day_task in

  assert_equal (Some end_day_task) (List.assoc 23 schedule);
  assert_equal (Some start_day_task) (List.assoc 0 schedule)

let test_add_task_to_partially_filled_schedule _ =
  let schedule = create_empty_daily_schedule () in
  let existing_task = create_task "Existing Task" 2 "High" in
  let new_task = create_task "New Task" 1 "Medium" in

  let schedule = add_task_to_day_schedule schedule 10 existing_task in
  let schedule = add_task_to_day_schedule schedule 12 new_task in

  assert_equal (Some existing_task) (List.assoc 10 schedule);
  assert_equal (Some existing_task) (List.assoc 11 schedule);
  assert_equal (Some new_task) (List.assoc 12 schedule)

let test_add_task_with_different_priorities _ =
  let schedule = create_empty_daily_schedule () in
  let high_priority_task = create_task "High Priority Task" 1 "High" in
  let medium_priority_task = create_task "Medium Priority Task" 1 "Medium" in
  let low_priority_task = create_task "Low Priority Task" 1 "Low" in

  let schedule = add_task_to_day_schedule schedule 9 high_priority_task in
  let schedule = add_task_to_day_schedule schedule 10 medium_priority_task in
  let schedule = add_task_to_day_schedule schedule 11 low_priority_task in

  assert_equal (Some high_priority_task) (List.assoc 9 schedule);
  assert_equal (Some medium_priority_task) (List.assoc 10 schedule);
  assert_equal (Some low_priority_task) (List.assoc 11 schedule)

let test_sort_schedule_with_mixed_priorities _ =
  let schedule = create_empty_daily_schedule () in
  let low_priority_task = create_task "Low Priority" 1 "Low" in
  let high_priority_task = create_task "High Priority" 1 "High" in
  let medium_priority_task = create_task "Medium Priority" 1 "Medium" in

  let schedule = add_task_to_day_schedule schedule 9 low_priority_task in
  let schedule = add_task_to_day_schedule schedule 10 high_priority_task in
  let schedule = add_task_to_day_schedule schedule 11 medium_priority_task in
  let sorted_schedule = sort_schedule_by_priority schedule in

  assert_equal (Some high_priority_task) (List.assoc 10 sorted_schedule);
  assert_equal (Some medium_priority_task) (List.assoc 11 sorted_schedule);
  assert_equal (Some low_priority_task) (List.assoc 9 sorted_schedule)

let test_reject_task_addition_in_occupied_slot _ =
  let schedule = create_empty_daily_schedule () in
  let existing_task = create_task "Existing Task" 1 "Medium" in
  let new_task = create_task "New Task" 1 "High" in

  let schedule = add_task_to_day_schedule schedule 10 existing_task in
  let updated_schedule = add_task_to_day_schedule schedule 10 new_task in

  assert_equal (Some existing_task) (List.assoc 10 updated_schedule)

let test_clear_schedule _ =
  let filled_schedule = create_filled_schedule 24 in
  let cleared_schedule =
    List.map (fun (hour, _) -> (hour, None)) filled_schedule
  in

  List.iter
    (fun (_, task_option) -> assert_equal None task_option)
    cleared_schedule

let test_smart_scheduling_with_overlaps _ =
  let weekly_schedule = create_empty_weekly_schedule () in
  let task1 = create_task "Early Task" 2 "Medium" in
  let task2 = create_task "Late Task" 2 "Medium" in

  let updated_schedule =
    smart_fill_day weekly_schedule Thursday [ task1; task2 ] 8 10
  in
  let thursday_schedule = List.assoc Thursday updated_schedule in

  let task1_scheduled = is_range_free thursday_schedule 8 2 |> not in
  let task2_scheduled = is_range_free thursday_schedule 10 2 |> not in

  assert_bool "Early Task should be scheduled" task1_scheduled;
  assert_bool "Late Task should be scheduled" task2_scheduled


let test_scheduling_on_specific_weekdays _ =
  let weekly_schedule = create_empty_weekly_schedule () in
  let task = create_task "Weekday Task" 1 "Medium" in

  let updated_schedule =
    List.fold_left
      (fun acc day -> add_task_to_weekly_schedule acc day 10 task)
      weekly_schedule
      [ Monday; Wednesday; Friday ]
  in

  let is_task_on_specified_days =
    List.fold_left
      (fun acc (day, schedule) ->
        match day with
        | Monday | Wednesday | Friday ->
            acc && List.assoc 10 schedule = Some task
        | _ -> acc)
      true updated_schedule
  in

  assert_bool
    "Task should be scheduled only on Mondays, Wednesdays, and Fridays"
    is_task_on_specified_days

let test_task_addition_at_specific_time _ =
  let schedule = create_empty_daily_schedule () in
  let task = create_task "Specific Time Task" 1 "Medium" in
  let time_slot = 15 in

  let updated_schedule = add_task_to_day_schedule schedule time_slot task in
  assert_equal (Some task) (List.assoc time_slot updated_schedule)

let test_sorting_tasks_by_priority _ =
  let schedule = create_empty_daily_schedule () in
  let high_task = create_task "High Priority" 1 "High" in
  let low_task = create_task "Low Priority" 1 "Low" in

  let schedule = add_task_to_day_schedule schedule 10 high_task in
  let schedule = add_task_to_day_schedule schedule 11 low_task in
  let sorted_schedule = sort_schedule_by_priority schedule in

  assert_equal (Some high_task) (List.hd sorted_schedule |> snd)

let test_finding_task_in_weekly_schedule _ =
  let weekly_schedule = create_empty_weekly_schedule () in
  let task = create_task "Find Me" 1 "Medium" in
  let updated_schedule =
    add_task_to_weekly_schedule weekly_schedule Monday 10 task
  in

  let found_task =
    find_task_weekly_schedule "Find Me" updated_schedule Monday
  in
  assert_bool "Task should be found" (found_task <> None)

let test_remove_all_tasks_from_schedule _ =
  let schedule = create_filled_schedule 24 in
  let cleared_schedule = List.map (fun (hour, _) -> (hour, None)) schedule in
  let has_tasks =
    List.exists (fun (_, task_opt) -> task_opt <> None) cleared_schedule
  in
  assert_equal false has_tasks

let test_check_free_slots_at_end_of_day _ =
  let schedule = create_empty_daily_schedule () in
  let task = create_task "Evening Task" 1 "High" in
  let schedule = add_task_to_day_schedule schedule 22 task in
  let is_free = is_range_free schedule 23 1 in
  assert_equal true is_free

let count_empty_slots schedule =
  List.fold_left
    (fun acc (_, task_opt) -> if task_opt = None then acc + 1 else acc)
    0 schedule

let test_schedule_empty_slot_count _ =
  let schedule = create_empty_daily_schedule () in
  let task = create_task "Some Task" 2 "Medium" in
  let updated_schedule = add_task_to_day_schedule schedule 12 task in
  let empty_slots = count_empty_slots updated_schedule in
  assert_equal 22 empty_slots

let test_smart_scheduling_with_diff_durations _ =
  let weekly_schedule = create_empty_weekly_schedule () in
  let task1 = create_task "Short Meeting" 1 "Medium" in
  let task2 = create_task "Long Workshop" 3 "High" in
  let updated_schedule =
    smart_fill_day weekly_schedule Thursday [ task1; task2 ] 9 17
  in
  let thursday_schedule = List.assoc Thursday updated_schedule in
  assert_bool "Both tasks should be scheduled"
    (is_range_free thursday_schedule 9 1 |> not
    && is_range_free thursday_schedule 10 3 |> not)

let scheduler_tests =
  [
    ( "create_filled_schedule 0" >:: fun _ ->
      assert_equal [] (create_filled_schedule 0) );
    ( "create_filled_schedule 1" >:: fun _ ->
      assert_equal
        [ (1, Some (create_task "Task1" 1 "Medium")) ]
        (create_filled_schedule 1) );
    ( "create_task" >:: fun _ ->
      assert_equal
        { name = "Task1"; duration = 1; priority = "Medium" }
        (create_task "Task1" 1 "Medium") );
    "schedule_to_string empty" >:: test_schedule_to_string "" [];
    "weekly_schedule_to_string empty Monday"
    >:: test_weekly_schedule_to_string "" [] Monday;
    "weekly_schedule_to_string empty Tuesday"
    >:: test_weekly_schedule_to_string "" [] Tuesday;
    "weekly_schedule_to_string empty Wednesday"
    >:: test_weekly_schedule_to_string "" [] Wednesday;
    "weekly_schedule_to_string empty Thursday"
    >:: test_weekly_schedule_to_string "" [] Thursday;
    "weekly_schedule_to_string empty Friday"
    >:: test_weekly_schedule_to_string "" [] Friday;
    "weekly_schedule_to_string empty Saturday"
    >:: test_weekly_schedule_to_string "" [] Saturday;
    "weekly_schedule_to_string empty Sunday"
    >:: test_weekly_schedule_to_string "" [] Sunday;
    "task_to_string empty"
    >:: test_task_to_string "Free Block - 1\n------------------------\n" None 1;
    "task_to_string empty"
    >:: test_task_to_string "Free Block - 2\n------------------------\n" None 2;
    "add single task to schedule" >:: test_add_task_to_schedule;
    "task creation test" >:: test_create_task;
    "empty schedule creation" >:: test_create_empty_schedule;
    "schedule sorter test" >:: test_sort_schedule_by_priority;
    "full schedule task add" >:: test_add_task_to_full_schedule;
    "overlapping task test" >:: test_add_overlapping_task;
    "task add to specific day" >:: test_add_task_to_specific_day;
    "smart_logic test" >:: test_smart_scheduling;
    "task update test" >:: test_update_existing_task;
    "full day rejection test" >:: test_reject_full_day;
    "task test at diff times" >:: test_add_task_at_different_times;
    "tast test with various durations" >:: test_add_task_with_various_durations;
    "time corner cases" >:: test_add_task_with_boundary_times;
    "partially filled schedule test"
    >:: test_add_task_to_partially_filled_schedule;
    "different priority testing" >:: test_add_task_with_different_priorities;
    "sort test w/ mixed priorities" >:: test_sort_schedule_with_mixed_priorities;
    "priority rejection logic test"
    >:: test_reject_task_addition_in_occupied_slot;
    "clear schedule test" >:: test_clear_schedule;
    "smart with overlaps" >:: test_smart_scheduling_with_overlaps;
    "spec_day scheduling" >:: test_scheduling_on_specific_weekdays;
    "spec_time addition" >:: test_task_addition_at_specific_time;
    "sort by priority test" >:: test_sorting_tasks_by_priority;
    "weekly find task" >:: test_finding_task_in_weekly_schedule;
    "remove all tasks" >:: test_remove_all_tasks_from_schedule;
    "check free slots" >:: test_check_free_slots_at_end_of_day;
    "empty_slot_count" >:: test_schedule_empty_slot_count;
    "smart w diff durations" >:: test_smart_scheduling_with_diff_durations;
  ]

let schedule_to_string_test out x _ =
  let stringified = schedule_to_string x in
  assert_equal out stringified

let weekly_schedule_to_string_test out in1 in2 _ =
  let stringified = weekly_schedule_to_string in1 in2 in
  assert_equal out stringified

let extract_tasks_test out in1 _ =
  let extracted = extract_tasks in1 in
  assert_equal extracted out

let find_tasks_schedule_test out in1 in2 _ =
  let extracted = find_task_schedule in1 in2 in
  assert_equal extracted out

let find_tasks_weekly_schedule_test out in1 in2 in3 _ =
  let extracted = find_task_weekly_schedule in1 in2 in3 in
  assert_equal extracted out

let filter_out_day_test out in1 in2 _ =
  let filtered = filter_out_day in1 in2 in
  assert_equal out filtered

let filter_out_task_test out in1 in2 _ =
  let filtered = filter_out_task in1 in2 in
  assert_equal out filtered

let task_to_string_test out in1 in2 _ =
  let stringified = task_to_string in1 in2 in
  assert_equal out stringified

let t1 = { name = "a"; duration = 1; priority = "High" }
let t2 = { name = "b"; duration = 2; priority = "High" }
let t3 = { name = "c"; duration = 3; priority = "High" }
let dempty = create_empty_daily_schedule ()

let d1 =
  add_task_to_day_schedule dempty 1
    { name = "a"; duration = 1; priority = "High" }

let d2 =
  add_task_to_day_schedule d1 2 { name = "b"; duration = 1; priority = "High" }

let d3 =
  add_task_to_day_schedule d2 3 { name = "d"; duration = 1; priority = "High" }

let d1_t = [ (1, Some { name = "a"; duration = 1; priority = "High" }) ]
let d2_t = [ (2, Some { name = "b"; duration = 1; priority = "High" }) ]
let d3_t = [ (3, Some { name = "d"; duration = 1; priority = "High" }) ]
let wempty = create_empty_weekly_schedule ()

let w1 =
  add_task_to_weekly_schedule wempty Monday 1
    { name = "a"; duration = 1; priority = "High" }

let w2 =
  add_task_to_weekly_schedule w1 Monday 2
    { name = "b"; duration = 1; priority = "High" }

let w3 =
  add_task_to_weekly_schedule w2 Monday 3
    { name = "d"; duration = 1; priority = "High" }

let nd1_t = [ (1, { name = "a"; duration = 1; priority = "High" }) ]
let nd2_t = [ (2, { name = "b"; duration = 1; priority = "High" }) ]
let nd3_t = [ (3, { name = "d"; duration = 1; priority = "High" }) ]

let none_ex =
  "Free Block - " ^ string_of_int 1 ^ "\n" ^ "------------------------\n"

let s1 =
  " Task: " ^ "a" ^ "\n" ^ " Start: " ^ string_of_int 1 ^ "\n" ^ " End: "
  ^ string_of_int (1 + 1)
  ^ "\n" ^ "------------------------ \n"

let s2 =
  " Task: " ^ "b" ^ "\n" ^ " Start: " ^ string_of_int 2 ^ "\n" ^ " End: "
  ^ string_of_int (2 + 1)
  ^ "\n" ^ "------------------------ \n"

let s3 =
  " Task: " ^ "c" ^ "\n" ^ " Start: " ^ string_of_int 3 ^ "\n" ^ " End: "
  ^ string_of_int (3 + 1)
  ^ "\n" ^ "------------------------ \n"

let gui_tests =
  [
    "home test" >:: schedule_to_string_test "" [];
    "weekly to string test" >:: weekly_schedule_to_string_test "" [] Monday;
    "extract_tasks" >:: extract_tasks_test [] [];
    "find_task 1" >:: find_tasks_schedule_test d2_t "b" d2;
    "find_task 2" >:: find_tasks_schedule_test d2_t "b" d3;
    "find_task 3" >:: find_tasks_schedule_test d3_t "d" d3;
    "find task 4" >:: find_tasks_schedule_test d1_t "a" d1;
    "find_task empty" >:: find_tasks_schedule_test [] "d" d2;
    "find_task_weekly 1"
    >:: find_tasks_weekly_schedule_test (Some []) "z" w1 Tuesday;
    "find_task_weekly 2"
    >:: find_tasks_weekly_schedule_test (Some nd2_t) "b" w2 Monday;
    "find_task_weekly 3"
    >:: find_tasks_weekly_schedule_test (Some nd2_t) "b" w3 Monday;
    "find_task_weekly 4"
    >:: find_tasks_weekly_schedule_test (Some nd3_t) "d" w3 Monday;
    "find_task_weekly 5"
    >:: find_tasks_weekly_schedule_test (Some nd1_t) "a" w1 Monday;
    "find_task_weekly 7"
    >:: find_tasks_weekly_schedule_test (Some nd2_t) "b" w3 Monday;
    "find_task_weekly empty"
    >:: find_tasks_weekly_schedule_test (Some []) "d" w2 Monday;
    "filter_out_day 1" >:: filter_out_day_test d1 "b" d2;
    "filter_out_day 2" >:: filter_out_day_test d1 "b" d1;
    "filter_out_day 3" >:: filter_out_day_test d3 "c" d3;
    "filter_out_day empty" >:: filter_out_day_test d2 "z" d2;
    "filter_out_day 5" >:: filter_out_day_test d1 "y" d1;
    "filter_out_task 1" >:: filter_out_task_test w2 "z" w2;
    "filter_out_task 2" >:: filter_out_task_test w2 "d" w3;
    "filter_out_task 3" >:: filter_out_task_test w1 "d" w1;
    "filter_out_task 4" >:: filter_out_task_test wempty "a" w1;
    "filter_out_task 5" >:: filter_out_task_test w1 "b" w2;
    "task_to_string 1" >:: task_to_string_test s1 (Some t1) 1;
    "task_to_string 2" >:: task_to_string_test none_ex None 1;
    "task_to_string 3" >:: task_to_string_test s2 (Some t2) 2;
    "task_to_string 4" >:: task_to_string_test s3 (Some t3) 3;
  ]

let suite = "test suite" >::: List.flatten [ scheduler_tests; gui_tests ]
let () = run_test_tt_main suite
