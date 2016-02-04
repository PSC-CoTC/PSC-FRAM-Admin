SELECT RunID.RunID as "run_id", 
  RunID.RunYear as "run_year",
  Escapement.StockID as "stock_id", 
  SUM(Escapement.Escapement) AS "escapement"
FROM (Escapement INNER JOIN RunID ON Escapement.RunID = RunID.RunID) 
WHERE RunID.RunName = %RUNNAME%
GROUP BY RunID.RunID, RunID.RunYear, Escapement.StockID;


