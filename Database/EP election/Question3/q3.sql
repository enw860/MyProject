SET SEARCH_PATH to parlgov;

-- drop q3 if pre exists
DROP TABLE IF EXISTS q3 CASCADE;
DROP VIEW IF ratio_in_peroid, qualified_rows,qualified_country;

CREATE TABLE q3 (
	countryName VARCHAR(50),
	year INT,
	participationRatio REAL
);

CREATE VIEW ratio_in_peroid AS
SELECT  
c.name,
date_part('year',e.e_date) AS "year",
AVG(1.0*e.votes_cast/e.electorate) AS "ratio"
FROM country c, election e
WHERE
e.votes_cast IS NOT NULL AND e.electorate IS NOT NULL AND
e.country_id = c.id AND
e.e_date >= '2001.01.01' AND e.e_date < '2017.01.01'
GROUP BY c.name,date_part('year',e.e_date);

CREATE VIEW qualified_rows AS
SELECT rip.name, rip.year, rip.ratio
FROM ratio_in_peroid rip
WHERE rip.ratio > ALL (
	SELECT t1.ratio FROM ratio_in_peroid t1
	WHERE t1.name = rip.name AND t1.year < rip.year
) AND rip.ratio < ALL (
	SELECT t2.ratio FROM ratio_in_peroid t2
	WHERE t2.name = rip.name AND t2.year > rip.year
);

CREATE VIEW qualified_country AS
SELECT rip.name
FROM ratio_in_peroid rip
GROUP BY rip.name
HAVING count(rip.year) = (
	SELECT count(qr.year) FROM qualified_rows qr
	WHERE qr.name = rip.name
);

INSERT INTO q3
SELECT rip.name,rip.year,rip.ratio
FROM ratio_in_peroid rip, qualified_country qc
WHERE rip.name = qc.name AND
rip.ratio BETWEEN 0 AND 1; 



