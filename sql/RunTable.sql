SELECT RunID as "run_id", 
  SpeciesName as "species",
  RunYear as "run_year",
  RunName as "run_name",
  RunTitle as "run_title",
  RunTimeDate as "run_time"
FROM RunID
WHERE RunID.RunYear > 1980
AND RunID.SpeciesName = %SPECIESNAME%


