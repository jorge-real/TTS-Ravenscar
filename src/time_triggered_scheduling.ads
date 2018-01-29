------------------------------------------------------------
--
--  ADA REAL-TIME TIME-TRIGGERED SCHEDULING SUPPORT
--
--  @file time_triggered_scheduling.ads
--
--  @package Time_Triggered_Scheduling (SPEC)
--
--  @brief
--   Ada Real-Time Time Triggered Scheduling support: Time Triggered Scheduling
--
--   Ravenscar version
------------------------------------------------------------

pragma Profile (Ravenscar);
private with Ada.Real_Time.Timing_Events, System;


with Ada.Real_Time; use Ada.Real_Time;


generic

   Number_Of_Work_IDs : Positive;

package Time_Triggered_Scheduling is

   --  Work identifier types
   type Any_Work_Id is new Integer range Integer'First .. Number_Of_Work_IDs;
   subtype Special_Work_Id is Any_Work_Id range Any_Work_Id'First .. 0;
   subtype Regular_Work_Id is Any_Work_Id range 1 .. Any_Work_Id'Last;

   --  Special IDs
   Empty_Slot       : constant Special_Work_Id;
   Mode_Change_Slot : constant Special_Work_Id;

   --  A time slot in the TT plan
   type Time_Slot is record
      Slot_Duration : Time_Span;
      Work_Id : Any_Work_Id;
      Next_Slot_Separation : Time_Span := Time_Span_Zero;
      --  User_Defined_Info: Any_User_Defined_Info;
   end record;

   --  Types representing/accessing TT plans
   type Time_Triggered_Plan is array (Natural range <>) of Time_Slot;
   type Time_Triggered_Plan_Access is access all Time_Triggered_Plan;


   --  Set new TT plan to start at the end of the next mode change slot
   procedure Set_Plan (TTP       : in Time_Triggered_Plan_Access);


   --  TT works use this procedure to wait for their next assigned slot
   --  The When_Was_Released result informs caller of slot starting time
   procedure Wait_For_Activation (Work_Id : Regular_Work_Id;
                                  When_Was_Released : out Time);


private
   use Ada.Real_Time.Timing_Events;

   Empty_Slot       : constant Special_Work_Id := 0;
   Mode_Change_Slot : constant Special_Work_Id := -1;


   protected Time_Triggered_Scheduler
     with Priority => System.Interrupt_Priority'Last is

      --  Setting a new TT plan
      procedure Set_Plan (TTP     : in Time_Triggered_Plan_Access);

   private
      --  New slot timing event
      NS_Event               : Timing_Event;
      --  New slot handler procedure
      procedure NS_Handler (Event : in out Timing_Event);

      --  This access object is the reason why the scheduler is declared
      --  in this private part, given that this is a generioc package.
      --  It should be a constant, but a PO can't have constant components.
      NS_Handler_Access  : Timing_Event_Handler := NS_Handler'Access;

      --  Procedure to enforce plan change
      procedure Change_Plan (At_Time : Time);

      --  Currently running plan and next plan to switch to, if any
      Current_Plan           : Time_Triggered_Plan_Access := null;
      Next_Plan              : Time_Triggered_Plan_Access := null;

      --  Index numbers of current and next slots in the plan
      Current_Slot_Index     : Natural := 0;
      Next_Slot_Index        : Natural := 0;

      --  Start time of next slot
      Next_Slot_Release      : Time    := Time_Last;

   end Time_Triggered_Scheduler;

end Time_Triggered_Scheduling;
