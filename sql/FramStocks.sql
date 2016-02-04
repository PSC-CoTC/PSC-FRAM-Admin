SELECT StockID as "stock_id", 
  StockName as "stock_name",
  StockLongName as "stock_long_name"
FROM Stock
WHERE UCASE(species) = 'COHO'


