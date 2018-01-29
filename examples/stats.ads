
with Ada.Real_Time; use Ada.Real_Time;

generic
   NTasks: Positive;
package Stats is

   subtype StatId is Natural range 1..NTasks*2;

   procedure Register_Time(Id: in StatId; Time: Time_Span);
   procedure Print_Stats;

private
   Histogram_Last : constant Natural := 46;
   type Histogram_Type is array (0..Histogram_Last) of Natural;
   type Histogram_Start is array (0..Histogram_Last) of Duration;

   type Stats_Type is record
      Minimum: Time_Span := Time_Span_Last;
      Maximum: Time_Span := Time_Span_First;
      Sum: Time_Span := Time_Span_Zero;
      Num: Integer := -10;
      Histogram: Histogram_Type := (others => 0);
      Start: Histogram_Start := (others => 0.0);
   end record;

   Stats: array (StatId) of Stats_Type;

end Stats;
