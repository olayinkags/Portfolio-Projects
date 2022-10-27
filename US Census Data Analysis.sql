SELECT *
FROM Project..county_data

SELECT * 
FROM Project..census_tract_data


--DATA CLEANING
--Detecting Duplicates
SELECT  COUNT (*)
FROM 
	(   SELECT CensusId, State, County, COUNT(*) AS  Records
		FROM Project..county_data
		GROUP BY CensusId, State, County
	)a
WHERE Records > 1


-- Top 10 States with the highest unemployemnt 
SELECT Top 10 State,
	   SUM(TotalPop - Employed) AS unemployment
FROM Project..county_data
GROUP BY State
ORDER BY 2 DESC


-- County with the Highest unemployment in the state with highest unemployment(California)
SELECT Top 1 County,
	   SUM(TotalPop - Employed) AS unemployment
FROM Project..county_data
WHERE State = 'California'
GROUP BY County
ORDER BY unemployment DESC


-- Toatl number of females and Males in each States
SELECT State,
	   SUM(Men) AS Total_number_males,
	   SUM(Women) AS Total_number_females
FROM Project..county_data
GROUP BY State


-- Total Population of different races in each state
SELECT a.State, 
	   sum(a.Total_population) AS Total_population,
	   sum(a.Hispanic_popuation) AS  Total_Hispanic_popuation,
	   sum(a.White_popuation) AS Total_White_popuation,
	   sum(a.Black_popuation) AS Total_Black_popuation,
	   sum(a.Native_popuation) AS Total_Native_popuation,
	   sum(a.Asian_popuation) AS Total_Asian_popuation,
	   sum(a.Pacific_popuation) AS Total_Pacific_popuation
FROM
	(SELECT State,
		TotalPop AS Total_population,
		ROUND((TotalPop * Hispanic)/100, 0) AS Hispanic_popuation,
		ROUND((TotalPop * White)/100, 0) AS White_popuation,
		ROUND((TotalPop * Black)/100, 0) AS Black_popuation,
		ROUND((TotalPop * Native)/100, 0) AS Native_popuation,
		ROUND((TotalPop * Asian)/100, 0) AS Asian_popuation,
		ROUND((TotalPop * Pacific)/100, 0) AS Pacific_popuation
FROM Project..County_data)a
GROUP BY State
ORDER BY Total_population DESC


-- Total poverty_population and Total population without poverty
SELECT b.State,
       b.Population,
	   SUM(b.poverty_population) AS Total_poverty_population,
	   SUM(b.population_without_poverty) AS Total_population_without_poverty
FROM
	(SELECT a.State,
	   a.Population,
	   ROUND(a.Poverty_rate * Population,0) AS poverty_population,
	   ROUND((1- a.Poverty_rate) * Population,0) AS population_without_poverty
	FROM
	(SELECT State,
	   Poverty/100 AS Poverty_rate,
	   TotalPop AS Population
	FROM Project..County_data)a )b
GROUP BY b.State, b.Population
ORDER BY b.Population DESC


-- Top 10 State with the highest average poverty rate and  compared with average unemployment rate
SELECT Top 10 State,
	ROUND(avg(Poverty),2) AS Average_poverty_rate,
	ROUND(avg(Unemployment),2) AS Average_unemployment_rate
FROM Project..County_data
GROUP BY State
ORDER BY Average_poverty_rate DESC

--Top 3 county in each state with highest income
SELECT a.*
FROM
	(SELECT State,
	   County,
	   Income,
	   RANK() OVER (PARTITION BY County ORDER BY Income DESC) Income_rank
	FROM Project..county_data)a
WHERE a.Income_rank IN (1, 2, 3)
ORDER BY a.County 
