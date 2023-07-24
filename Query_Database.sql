USE AdventureWorksDW2019;

--GROUPING SETS clause
SELECT
  pc.EnglishProductCategoryName,
  psc.EnglishProductSubcategoryName,
  p.EnglishProductName,
  SUM(fis.SalesAmount) AS TotalSales
FROM FactInternetSales AS fis
JOIN DimProduct AS p ON fis.ProductKey = p.ProductKey
JOIN DimProductSubcategory AS psc ON p.ProductSubcategoryKey = psc.ProductSubcategoryKey
JOIN DimProductCategory AS pc ON psc.ProductCategoryKey = pc.ProductCategoryKey
GROUP BY GROUPING SETS (
  (pc.EnglishProductCategoryName, psc.EnglishProductSubcategoryName, p.EnglishProductName),
  (pc.EnglishProductCategoryName, psc.EnglishProductSubcategoryName),
  (pc.EnglishProductCategoryName),
  ()
)
ORDER BY pc.EnglishProductCategoryName, psc.EnglishProductSubcategoryName, p.EnglishProductName;
--Total sales amount = 29,358,677.22
--151 rows

--CUBE clause
SELECT 
  pc.EnglishProductCategoryName,
  psc.EnglishProductSubcategoryName,
  p.EnglishProductName,
  SUM(fis.SalesAmount) AS TotalSales
FROM FactInternetSales AS fis
JOIN DimProduct AS p ON fis.ProductKey = p.ProductKey
JOIN DimProductSubcategory AS psc ON p.ProductSubcategoryKey = psc.ProductSubcategoryKey
JOIN DimProductCategory AS pc ON psc.ProductCategoryKey = pc.ProductCategoryKey
GROUP BY CUBE (
  pc.EnglishProductCategoryName,
  psc.EnglishProductSubcategoryName,
  p.EnglishProductName
)
ORDER BY pc.EnglishProductCategoryName, psc.EnglishProductSubcategoryName, p.EnglishProductName;
--Total sales amount = 29,358,677.22
--558 rows

--ROLLUP clause
SELECT 
  pc.EnglishProductCategoryName,
  psc.EnglishProductSubcategoryName,
  p.EnglishProductName,
  SUM(fis.SalesAmount) AS TotalSales
FROM FactInternetSales AS fis
JOIN DimProduct AS p ON fis.ProductKey = p.ProductKey
JOIN DimProductSubcategory AS psc ON p.ProductSubcategoryKey = psc.ProductSubcategoryKey
JOIN DimProductCategory AS pc ON psc.ProductCategoryKey = pc.ProductCategoryKey
GROUP BY ROLLUP (
  pc.EnglishProductCategoryName,
  psc.EnglishProductSubcategoryName,
  p.EnglishProductName
)
ORDER BY pc.EnglishProductCategoryName, psc.EnglishProductSubcategoryName, p.EnglishProductName;
--Total sales amount = 29,358,677.22
--151 rows


--ROLLUP clause & GROUPING function
SELECT 
  pc.EnglishProductCategoryName,
  psc.EnglishProductSubcategoryName,
  p.EnglishProductName,
  SUM(fis.SalesAmount) AS TotalSales,
  GROUPING(pc.EnglishProductCategoryName) AS GroupingCategory,
  GROUPING(psc.EnglishProductSubcategoryName) AS GroupingSubcategory
FROM FactInternetSales AS fis
JOIN DimProduct AS p ON fis.ProductKey = p.ProductKey
JOIN DimProductSubcategory AS psc ON p.ProductSubcategoryKey = psc.ProductSubcategoryKey
JOIN DimProductCategory AS pc ON psc.ProductCategoryKey = pc.ProductCategoryKey
GROUP BY ROLLUP (
  pc.EnglishProductCategoryName,
  psc.EnglishProductSubcategoryName,
  p.EnglishProductName
)
ORDER BY pc.EnglishProductCategoryName, psc.EnglishProductSubcategoryName, p.EnglishProductName;
--Total sales amount = 29,358,677.22
--151 rows


--ROLLUP clause & GROUPING_ID function
SELECT 
  pc.EnglishProductCategoryName,
  psc.EnglishProductSubcategoryName,
  p.EnglishProductName,
  SUM(fis.SalesAmount) AS TotalSales,
  GROUPING_ID(pc.EnglishProductCategoryName, psc.EnglishProductSubcategoryName, p.EnglishProductName) AS GROUPINGID
FROM FactInternetSales AS fis
JOIN DimProduct AS p ON fis.ProductKey = p.ProductKey
JOIN DimProductSubcategory AS psc ON p.ProductSubcategoryKey = psc.ProductSubcategoryKey
JOIN DimProductCategory AS pc ON psc.ProductCategoryKey = pc.ProductCategoryKey
GROUP BY ROLLUP (
  pc.EnglishProductCategoryName,
  psc.EnglishProductSubcategoryName,
  p.EnglishProductName
)
ORDER BY pc.EnglishProductCategoryName, psc.EnglishProductSubcategoryName, p.EnglishProductName;
--Total sales amount = 29,358,677.22
--151 rows

