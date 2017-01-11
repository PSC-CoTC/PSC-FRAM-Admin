SELECT DISTINCT RunID.RunID as run_id,
       Fishery.FisheryName as fishery_name,
	     BaseExploitationRate.FisheryID as fishery_id,
       BaseExploitationRate.TimeStep as time_step
FROM RunID 
      INNER JOIN (BaseExploitationRate INNER JOIN Fishery 
        ON BaseExploitationRate.FisheryID = Fishery.FisheryID )
      ON RunID.BasePeriodID = BaseExploitationRate.BasePeriodID 
WHERE (Fishery.Species='COHO' AND RunID.SpeciesName='COHO' AND RunID.RunName = %RUNNAME%);
