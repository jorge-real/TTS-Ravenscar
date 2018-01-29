with Logging_Support; use Logging_Support;

package body Stats is

   function TS_Div_Floor(T1, T2: Time_Span) return Natural is
     (Natural(Float'Floor(Float(To_Duration(T1)) / Float(To_Duration(T2)))));

   CRLF : constant String := Character'Val(13) & Character'Val(10);
   CR   : constant String := CRLF(CRLF'First..CRLF'First);
   -------------------
   -- Register_Time --
   -------------------

   procedure Register_Time (Id: in StatId; Time: Time_Span) is
      Base : Natural;
      Index : Natural;
      Start : Duration;
      Width : Duration;
   begin
      if Time < Time_Span_Zero then
         return;
      end if;

      Stats(Id).Num := Stats(Id).Num + 1;
      if (Stats(Id).Num <= 0) then
         return;
      end if;

      if Stats(Id).Minimum > Time then
         Stats(Id).Minimum := Time;
      end if;

      if Stats(Id).Maximum < Time then
         Stats(Id).Maximum := Time;
      end if;

      Stats(Id).Sum := Stats(Id).Sum + Time;

      if Time < Microseconds(100) then
         Base := 0;
         Index := TS_Div_Floor(Time, Microseconds(10));
         Start := 0.000_01;
         Width := 0.000_01;
      elsif Time < Milliseconds(1) then
         Base := 9;
         Index := TS_Div_Floor(Time, Microseconds(100));
         Start := 0.000_1;
         Width := 0.000_1;
      elsif Time < Milliseconds(10) then
         Base := 18;
         Index := TS_Div_Floor(Time, Milliseconds(1));
         Start := 0.001;
         Width := 0.001;
      elsif Time < Milliseconds(100) then
         Base := 27;
         Index := TS_Div_Floor(Time, Milliseconds(10));
         Start := 0.01;
         Width := 0.01;
      elsif Time < Seconds(1) then
         Base := 36;
         Index := TS_Div_Floor(Time, Milliseconds(100));
         Start := 0.1;
         Width := 0.1;
      else
         Base := 45;
         Index := 1;
         Start := 1.0;
         Width := 1.0;
      end if;

      Stats(Id).Histogram(Base + Index) := Stats(Id).Histogram(Base + Index) + 1;
      Stats(Id).Start(Base + Index) := Start + (Index-1)*Width;

   end Register_Time;

   -----------------
   -- Print_Stats --
   -----------------

   procedure Print_Stats is
      Average: Duration;
      Break_Line : Boolean;
   begin
      for Id in StatId'Range loop
         Log(No_Event, "Task: " & StatId'Image(Id));
         Log(No_Event, "    Min: " & Duration'Image(To_Duration(Stats(Id).Minimum)) &
               "    Max: " & Duration'Image(To_Duration(Stats(Id).Maximum)) &
               "    Sum: " & Duration'Image(To_Duration(Stats(Id).Sum)) &
               "    Num: " & StatId'Image(Stats(Id).Num));
         if (Stats(Id).Num > 0) then
            Average := To_Duration(Stats(Id).Sum) / Stats(Id).Num;
            Log(No_Event, "    Avg: " & Duration'Image(Average));
         end if;
         Log (No_Event,CRLF);

         Break_Line := False;
         for Index in 0 .. Histogram_Last loop
            if (Stats(Id).Histogram(Index) > 0) then
               Log(No_Event, "     H("& Integer'Image(Index) & ":" &Duration'Image(Stats(Id).Start(Index)) &"): " & Natural'Image(Stats(Id).Histogram(Index)) & ", ");
               if Break_Line then
                  Log (No_Event,CR);
               end if;
               Break_Line := not Break_Line;
            end if;

         end loop;
         Log (No_Event,CRLF);

         delay until Clock + Seconds (10); --10.0;
         for I in 1..25 loop
            Log (No_Event,CRLF);
         end loop;

      end loop;

   end Print_Stats;


end Stats;
