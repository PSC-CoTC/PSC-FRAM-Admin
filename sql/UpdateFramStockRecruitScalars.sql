SELECT 
  StockRecruit.RecruitScaleFactor = %RECRUITSCALAR%
FROM StockRecruit
WHERE StockRecruit.RunID = %RUNID% AND StockRecruit.StockID = %STOCKID%;
