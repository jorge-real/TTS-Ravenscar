------------------------------------------------------------
--
--  ADA REAL-TIME TIME-TRIGGERED SCHEDULING SUPPORT
--
--  @file time_triggered_scheduling.adb
--
--  @package Time_Triggered_Scheduling (BODY)
--
--  @brief
--   Ada Real-Time Time Triggered Scheduling support : Time Triggered Scheduling
--
--   Ravenscar version
--
------------------------------------------------------------

with Ada.Task_Identification;      use Ada.Task_Identification;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

package body Time_Triggered_Scheduling is

   --  Run time TT work info
   type Work_Control_Block is record
      Release_Point : Suspension_Object;
      Is_Running    : Boolean := False;
      Work_Task_Id  : Task_Id := Null_Task_Id;
      Last_Release  : Time    := Time_Last;
   end record;

   --  Array of Work_Control_Blocks
   WCB : array (Regular_Work_Id) of Work_Control_Block;


   --------------------------
   --  Wait_For_Activation --
   --------------------------
   procedure Wait_For_Activation (Work_Id : Regular_Work_Id;
                                  When_Was_Released : out Time) is
   begin

      --  Register the Work_Id with the first task using it.
      --  Use of the Work_Id by another task breaks the model and causes PE
      if WCB (Work_Id).Work_Task_Id = Null_Task_Id then

         --  First time WFA called with this Work_Id -> Register caller
         WCB (Work_Id).Work_Task_Id := Current_Task;

      elsif  WCB (Work_Id).Work_Task_Id /= Current_Task then

         --  Caller was not registered with this Work_Id
         raise Program_Error with ("Work_Id misuse");

      end if;

      --  Caller is about to be suspended, hence not running
      WCB (Work_Id).Is_Running := False;

      Suspend_Until_True (WCB (Work_Id).Release_Point);

      --  Scheduler updated Last_Release when it released the worker task
      When_Was_Released := WCB (Work_Id).Last_Release;

      --  The worker is now released and starts running
      WCB (Work_Id).Is_Running := True;

   end Wait_For_Activation;


   ------------------------------
   -- Time_Triggered_Scheduler --
   ------------------------------

   protected body Time_Triggered_Scheduler is

      --------------
      -- Set_Plan --
      --------------

      procedure Set_Plan (TTP : in Time_Triggered_Plan_Access) is
         Now : constant Time := Clock;
      begin

         --  Take note of next plan to execute
         Next_Plan := TTP;

         --  Start new plan now if none is set. Otherwise, the scheduler will
         --  change to the Next_Plan at the end of the next mode change slot
         if Current_Plan = null then
            Change_Plan (Now);
            return;
         end if;

         --  Accept Set_Plan requests during a mode change slot (coming
         --  from ET tasks) and enforce the mode change at the end of it.
         --  Note that this point is reached only if there is currently
         --  a plan set, hence Current_Plan is not null
         if Current_Plan (Current_Slot_Index).Work_Id = Mode_Change_Slot then
            Change_Plan (Next_Slot_Release);
         end if;

      end Set_Plan;

      -----------------
      -- Change_Plan --
      -----------------

      procedure Change_Plan (At_Time : Time) is
      begin
         Current_Plan := Next_Plan;
         Next_Plan := null;
         --  Setting both Current_ and Next_Slot_Index to 'First is consistent
         --  with the new slot TE handler for the first slot of a new plan.
         Current_Slot_Index := Current_Plan.all'First;
         Next_Slot_Index := Current_Plan.all'First;
         Next_Slot_Release := At_Time;
         NS_Event.Set_Handler (At_Time, NS_Handler_Access);
      end Change_Plan;

      ----------------
      -- NS_Handler --
      ----------------

      procedure NS_Handler (Event : in out Timing_Event) is
         pragma Unreferenced (Event);
         Current_Work_Id : Any_Work_Id;
         Now             : Time;
      begin
         Now := Next_Slot_Release;

         --  Check for overrun in the finishing slot.
         --  If this happens to be the first slot after a plan change, then
         --  an overrun from the last work of the old plan would have been
         --  detected at the start of the mode change slot.
         Current_Work_Id := Current_Plan (Current_Slot_Index).Work_Id;
         if Current_Work_Id in Regular_Work_Id
           and then WCB (Current_Work_Id).Is_Running
         then
            --  Overrun detected, raise PE
            raise Program_Error with ("Overrun detected in work " & Current_Work_Id'Image);
         end if;

         --  Update current slot index
         Current_Slot_Index := Next_Slot_Index;

         --  Compute next slot index
         if Next_Slot_Index < Current_Plan.all'Last then
            Next_Slot_Index := Next_Slot_Index + 1;
         else
            Next_Slot_Index := Current_Plan.all'First;
         end if;

         --  Compute next slot start time
         Next_Slot_Release := Next_Slot_Release + Current_Plan (Current_Slot_Index).Slot_Duration;

         --  Obtain current work id
         Current_Work_Id := Current_Plan (Current_Slot_Index).Work_Id;

         --  Process current slot actions
         --  Mode change slot case --

         if Current_Work_Id = Mode_Change_Slot then
            if Next_Plan /= null then
               --  There's a pending plan change. It takes effect at the end of the MC slot
               Change_Plan (Next_Slot_Release);
            else
               --  Set the handler for the next scheduling point
               NS_Event.Set_Handler (Next_Slot_Release, NS_Handler_Access);
            end if;

         --  Empty slot case  --

         elsif Current_Work_Id = Empty_Slot then
            --  Set the handler for the next scheduling point
            NS_Event.Set_Handler (Next_Slot_Release, NS_Handler_Access);

         --  Regular slot case --

         elsif Current_Work_Id in Regular_Work_Id'Range then

            --  Check whether the work's task has already registered. Raise PE otherwise
            if WCB (Current_Work_Id).Work_Task_Id = Null_Task_Id then
               raise Program_Error with ("Task not registered for Work_Id " & Current_Work_Id'Image);
            end if;

            --  Support for release time to measure release jitter of TT tasks
            WCB (Current_Work_Id).Last_Release := Now;

            --  Release task in charge of current work id
            Set_True (WCB (Current_Work_Id).Release_Point);

            --  Set the handler for the next scheduling point
            NS_Event.Set_Handler (Next_Slot_Release, NS_Handler_Access);

         else
            raise Program_Error with "Invalid Work Id";
         end if;
      end NS_Handler;

   end Time_Triggered_Scheduler;


   ----------------
   --  Set_Plan  --
   ----------------

   procedure Set_Plan (TTP : in Time_Triggered_Plan_Access) is
   begin
      Time_Triggered_Scheduler.Set_Plan (TTP);
   end Set_Plan;

end Time_Triggered_Scheduling;
