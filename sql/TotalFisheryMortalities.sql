SELECT RunID.RunID as "run_id", 
  		 RunID.RunYear as "run_year", 
	   Mortality.FisheryID as "fishery_id",
       Mortality.StockID as "stock_id",
	   SUM (LandedCatch + NonRetention + Shaker + DropOff + MSFLandedCatch + MSFNonRetention + MSFShaker + MSFDropOff) AS "total_mortality" 
	FROM Mortality 
		INNER JOIN RunID ON Mortality.RunID = RunID.RunID
WHERE RunID.RunName = %RUNNAME%
	GROUP BY RunID.RunID, RunID.RunYear, Mortality.FisheryID, Mortality.StockID