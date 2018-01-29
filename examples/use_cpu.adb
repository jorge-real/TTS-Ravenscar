--  with Ada.Text_IO, Ada.Real_Time;
--  use  Ada.Text_IO, Ada.Real_Time;

package body Use_CPU is

--     T1, T2, T3 : Time;
--     Time_Measured : Time_Span;
--     Tolerance : Time_Span := Microseconds (50);
--     X : Float := 0.0;
--     Min : Integer := 0;
--     Max : Integer := 10_000_000;
--     NTimes : Integer := (Max - Min) / 2; -- Number of iterations to calibrate for 10 ms use of CPU

   procedure Iterate (Iterations : in Integer)
     with Inline
   is
      X : Float := 0.0;
   begin
      for I in 1 .. Iterations loop
         X := X + 16.25;
         X := X - 16.25;
         X := X + 16.25;
         X := X - 16.25;
         X := X + 16.25;
         X := X - 16.25;
      end loop;
   end Iterate;


   procedure Work (Amount_MS : in Natural) is
      --  Constant 27464 comes from several calibration runs on the STM32F4 Discovery board
      Iterations : Integer := (27464 * Amount_MS) / 10; --  (NTimes * Amount_MS) / 10;
   begin
      Iterate (Iterations);
   end Work;

--  begin
--     Put ("Calibrating nr. of iterations for 10 ms...");
--     loop
--        --Put("Trying" & Integer'Image(NTimes) & " times:");
--        --Put(" ." & Integer'Image(NTimes) );
--        Put (".");
--        T1 := Clock;
--        Iterate (NTimes);
--        T2 := Clock;
--        T3 := Clock;
--        Time_Measured := (T2 - T1 - (T3 - T2));
--        --Put("  Took" & Duration'Image(To_Duration(Time_Measured)) & " seconds.");
--        if abs (Time_Measured - Milliseconds (10)) <= Tolerance then
--           exit;
--        elsif (Time_Measured > Milliseconds (10)) then -- NTimes too large -> reduce
--           Max := NTimes;
--        else -- NTimes too short -> increase
--           Min := NTimes;
--        end if;
--        --Put_Line (" Searching now range" & Integer'Image(Min) & " .. " & Integer'Image(Max));
--        NTimes := Min + ((Max - Min) / 2);
--     end loop;
--     Put_Line (" Done!");
--     Put_Line("            Nr. of iterations for 10 ms =" & Integer'Image(NTimes));
--     New_Line;

end Use_CPU;
