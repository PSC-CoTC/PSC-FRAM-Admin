UPDATE NonRetention SET BackwardsFRAM.TargetFlag=%ESCAPEMENTFLAG%, 
       BackwardsFRAM.TargetEscAge3=%TARGETESCAPEMENT%,
       BackwardsFRAM.Comment=%COMMENT%
WHERE RunID=%RUNID% AND StockID=%STOCKID%;
