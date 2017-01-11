UPDATE FisheryScalers SET FisheryFlag=%FISHERYFLAG%, 
                          Quota=%NONSELECTIVECATCH%, 
                          MSFQuota=%MSFCATCH%, 
                          MarkReleaseRate=%MARKRELEASERATE%, 
                          MarkMisIDRate=%MARKMISIDRATE%, 
                          UnMarkMisIDRate=%UNMARKMISSIDRATE%, 
                          MarkIncidentalRate=%MARKINCIDENTALRATE%
WHERE FisheryScalers.RunID=%RUNID% AND FisheryScalers.FisheryID=%FISHERYID% AND FisheryScalers.TimeStep=%TIMESTEP%;
