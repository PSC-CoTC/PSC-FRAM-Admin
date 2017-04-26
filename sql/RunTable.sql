SELECT RunID as "fram_run_id", 
  SpeciesName as "species",
  RunYear as "fram_run_year",
  RunName as "fram_run_name",
  RunTitle as "fram_run_title",
  RunTimeDate as "fram_run_time"
FROM RunID
WHERE RunID.RunYear > 1980
AND RunID.SpeciesName = %SPECIESNAME%


