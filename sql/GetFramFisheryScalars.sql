SELECT RunID.RunID as "run_id", 
       RunID.RunName as "run_name", 
       FisheryScalers.FisheryID as "fishery_id", 
	   Fishery.FisheryName as "fishery_name", 
	   FisheryScalers.TimeStep as "time_step", 
	   FisheryScalers.FisheryFlag AS "fishery_flag", 
	   FisheryScalers.Quota AS "nonselective_catch", 
	   FisheryScalers.MSFQuota AS "msf_catch", 
	   NonRetention.CNRInput1 AS "cnr_mortalities",
	   FisheryScalers.MarkReleaseRate AS "mark_release_rate",
	   FisheryScalers.MarkMisIDRate AS "mark_missid_rate",
	   FisheryScalers.UnMarkMisIDRate AS "unmark_missid_rate",
	   FisheryScalers.MarkIncidentalRate AS "mark_incidental_rate"	   
FROM Fishery 
  RIGHT JOIN (NonRetention 
    RIGHT JOIN (RunID 
	  LEFT JOIN FisheryScalers ON RunID.RunID = FisheryScalers.RunID) 
	ON (NonRetention.TimeStep = FisheryScalers.TimeStep) 
	 AND (NonRetention.FisheryID = FisheryScalers.FisheryID) 
	 AND (NonRetention.RunID = FisheryScalers.RunID)) 
 ON Fishery.FisheryID = FisheryScalers.FisheryID
WHERE (Fishery.Species='COHO' AND RunID.SpeciesName='COHO' AND RunID.RunName = %RUNNAME%);
