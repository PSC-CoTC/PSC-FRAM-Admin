SELECT 
  BackwardsFRAM.TargetFlag as "escapement_flag"
FROM BackwardsFRAM
WHERE BackwardsFRAM.RunID = %RUNID% AND BackwardsFRAM.StockID = %STOCKID%;
