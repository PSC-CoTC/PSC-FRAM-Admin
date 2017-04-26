SELECT StockID as "fram_stock_id", 
  StockName as "fram_stock_name",
  StockLongName as "fram_stock_long_name"
FROM Stock
WHERE UCASE(species) = 'COHO'


