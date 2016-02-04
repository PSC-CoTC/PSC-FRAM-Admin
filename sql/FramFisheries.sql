SELECT FisheryID as "fishery_id", 
  FisheryName as "fishery_name",
  FisheryTitle as "fishery_long_name"
FROM Fishery
WHERE UCASE(species) = 'COHO'


