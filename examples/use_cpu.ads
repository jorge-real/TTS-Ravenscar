package Use_CPU is
   procedure Work (Amount_MS : in Natural)
     with Inline;
   --  Amount of time in ms, truncated to tens of ms
end Use_CPU;
