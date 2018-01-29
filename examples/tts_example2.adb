with System;
with Ada.Real_Time; use Ada.Real_Time;
with Time_Triggered_Scheduling; --  use Time_Triggered_Scheduling;




--  The following packages are for tracing and timing support
with Ada.Exceptions; use Ada.Exceptions;
with Logging_Support; use Logging_Support;
with Use_CPU; use Use_CPU;
with Ada.Text_IO; use Ada.Text_IO;
with Epoch_Support; use Epoch_Support;
with STM32.Board; use STM32.Board;

--  with Stats;

package body TTS_Example2 is

   --  package MyStats is new Stats (5);


   --  Instantiation of generic TT scheduler
   No_Of_TT_Works : constant := 3;
   package TT_Scheduler is new Time_Triggered_Scheduling (No_Of_TT_Works);
   use TT_Scheduler;


   function New_Slot (ms  : Natural;
                      WId : Any_Work_Id;
                      Slot_Separation : Natural := 0) return Time_Slot;

   function New_Slot (ms  : Natural;
                      WId : Any_Work_Id;
                      Slot_Separation : Natural := 0) return Time_Slot is
      Slot : Time_Slot;
   begin
      Slot.Slot_Duration := Milliseconds (ms);
      Slot.Work_Id := WId;
      Slot.Next_Slot_Separation := Milliseconds (Slot_Separation);
      return Slot;
   end New_Slot;


   ----------------------------
   --  Time-triggered plans  --
   ----------------------------

   TTP1 : aliased Time_Triggered_Plan :=
     (
      New_Slot (30, 1),
      New_Slot (70, Empty_Slot),
      New_Slot (60, 2),
      New_Slot (40, Empty_Slot),
      New_Slot (90, 3),
      New_Slot (10, Mode_Change_Slot)
     );

   TTP2 : aliased Time_Triggered_Plan :=
     (
      New_Slot (90, 3),
      New_Slot (10, Empty_Slot),
      New_Slot (60, 2),
      New_Slot (40, Empty_Slot),
      New_Slot (30, 1),
      New_Slot (70, Mode_Change_Slot)
     );

   Null_Plan : aliased Time_Triggered_Plan :=
     (
      0 => New_Slot (100, Empty_Slot),
      1 => New_Slot (100, Mode_Change_Slot)
     );


   -------------------
   -- Task Patterns --
   -------------------

   --  A basic TT task

   task type Basic_TT_Task (Work_Id : Regular_Work_Id;
                            Execution_Time_MS : Natural)
     with Priority => System.Priority'Last is
   end Basic_TT_Task;

   task body Basic_TT_Task is
      Work_To_Be_Done : constant Natural := Execution_Time_MS;
      LED_To_Turn : User_LED;
      When_Was_Released : Time;
--        Jitter : Time_Span;
   begin
      case Work_Id is
         when 1 =>
            LED_To_Turn := Red_LED;
         when 2 =>
            LED_To_Turn := Blue_LED;
         when 3 =>
            LED_To_Turn := Green_LED;
      end case;

      loop
         Wait_For_Activation (Work_Id, When_Was_Released);
--           Jitter := Clock - When_Was_Released;
--           Log (No_Event, "|---> Jitter of Worker" & Integer'Image (Integer (Work_Id)) &
--                         " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  MyStats.Register_Time(Integer(Work_Id)*2-1, Jitter);

         Set (Probe_TT_Point);
         Turn_On (LED_To_Turn);

         Work (Work_To_Be_Done);

         Turn_Off (LED_To_Turn);
         Clear (Probe_TT_Point);

         --  Log (Stop_Task, "W" & Character'Val (Character'Pos ('0') + Integer (Work_Id)));
      end loop;
   exception
      when E : others =>
         Put_Line ("Periodic worker W" & Character'Val (Character'Pos ('0') + Integer (Work_Id)) &
                     ": " & Exception_Message (E));
   end Basic_TT_Task;


   -------------------------------
   --  Priority scheduled tasks --
   -------------------------------

   task type DM_Task (Id : Natural; Period : Integer; Prio : System.Priority)
     with Priority => Prio;

   task body DM_Task is
      Next : Time := Epoch;
      Per : constant Time_Span := Milliseconds (Period);
      Jitter : Time_Span;
   begin
      loop
         delay until Next;
         Jitter := Clock - Next;
         Log (No_Event, "|---------> Jitter of DM Task" & Integer'Image (Id) &
                         " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  MyStats.Register_Time(Integer(Id)*2-1, Jitter);
         --  Log (Start_Task, "T" & Character'Val (Character'Pos ('0') + Integer (Id)));

         Set (Probe_ET_Point);
         Turn_On (Orange_LED);

         Work (5);

         Next := Next + Per;

         Turn_Off (Orange_LED);
         Clear (Probe_ET_Point);

         --  Log (Stop_Task, "T" & Character'Val (Character'Pos ('0') + Integer (Id)));
      end loop;
   exception
      when E : others =>
         Put_Line (Exception_Message (E));
   end DM_Task;


   --  Event-triggered tasks

   T4 : DM_Task (Id => 4, Period => 90, Prio => System.Priority'First + 1);
   T5 : DM_Task (Id => 5, Period => 210, Prio => System.Priority'First);

   --  Time-triggered tasks
   --  Work_ID, Execution (ms)
   Wk1 : Basic_TT_Task (1, 20);
   Wk2 : Basic_TT_Task (2, 40);
   Wk3 : Basic_TT_Task (3, 60);


   procedure Main is
      K : Integer := 0; --  Number of iterations in main loop
   begin
      -- Generate trace header --
      Log (No_Event, "1   M1"); -- Nr of modes
      Log (No_Event, "5");      -- Nr of works + Nr of tasks
      Log (No_Event, "W1  9.200 9.200 0.0 10"); -- Task_name Period Deadline Phasing Priority
      Log (No_Event, "W2  9.200 9.200 0.0 9");
      Log (No_Event, "W3  9.200 9.200 0.0 8");
      Log (No_Event, "T4  0.600 0.600 0.0 5");
      Log (No_Event, "T5  0.800 0.800 0.0 4");


      Log (No_Event, ":BODY");
      delay until Epoch;

      loop

         Log (Mode_Change, "M1");
         Set_Plan (TTP1'Access);
         delay until Epoch + Seconds (K * 30 + 10);

         Log (Mode_Change, "Null Plan");
         Set_Plan (Null_Plan'Access);
         delay until Epoch + Seconds (K * 30 + 15);

         Log (Mode_Change, "M2");
         Set_Plan (TTP2'Access);
         delay until Epoch + Seconds (K * 30 + 25);

         Log (Mode_Change, "Null Plan");
         Set_Plan (Null_Plan'Access);
         delay until Epoch + Seconds (K * 30 + 30);

         K := K + 1;

      end loop;
      --  MyStats.Print_Stats;

      --  delay until Time_Last;
   end Main;

   procedure Configure_Probe_Points;
   procedure Configure_Probe_Points is
      Configuration : GPIO_Port_Configuration;
   begin
      Configuration.Mode := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed := Speed_50MHz;
      Configuration.Resistors := Floating;
      Enable_Clock (Probe_TT_Point & Probe_ET_Point);
      Configure_IO (Probe_TT_Point & Probe_ET_Point, Configuration);
      Clear (Probe_TT_Point);
      Clear (Probe_ET_Point);
   end Configure_Probe_Points;


begin
   Configure_Probe_Points;
   Initialize_LEDs;
   All_LEDs_Off;
end TTS_Example2;
