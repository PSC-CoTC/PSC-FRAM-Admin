SELECT RunID.RunID as "run_id", 
  RunID.RunYear as "run_year",
  Escapement.StockID as "stock_id", 
  Escapement.TargetFlag as "escapement_flag"
  Escapement.TargetEscAge3 AS "escapement"
FROM (BackwardsFRAM INNER JOIN RunID ON BackwardsFRAM.RunID = RunID.RunID) 
WHERE RunID.RunName = %RUNNAME%;


