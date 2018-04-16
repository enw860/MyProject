SET SEARCH_PATH to parlgov;

-- drop q4 if pre exists
DROP TABLE IF EXISTS q4 CASCADE;
DROP VIEW IF EXISTS build_level,count_view_v,r0,r2,r4,r6,r8;

CREATE TABLE q4(
	countryName VARCHAR(50),
	r0_2 INT,
	r2_4 INT,
	r4_6 INT,
	r6_8 INT,
	r8_10 INT
);

CREATE VIEW build_level AS
SELECT c.name,p.name_short,
CASE
WHEN (pp.left_right>=0 AND pp.left_right<2) THEN 'r0_2'
WHEN (pp.left_right>=2 AND pp.left_right<4) THEN 'r2_4'
WHEN (pp.left_right>=4 AND pp.left_right<6) THEN 'r4_6'
WHEN (pp.left_right>=6 AND pp.left_right<8) THEN 'r6_8'
WHEN (pp.left_right>=8 AND pp.left_right<10) THEN 'r8_10'
END AS "level"
FROM country c,party_position pp,party p
WHERE p.id = pp.party_id AND p.country_id = c.id AND
pp.left_right IS NOT NULL;

CREATE VIEW count_view_v AS
SELECT name,level,COUNT(*) AS level_count
FROM build_level
GROUP BY name,level;

CREATE VIEW r0 AS
SELECT name, level_count AS "r0_2"
FROM  count_view_v
WHERE level = 'r0_2';

CREATE VIEW r2 AS
SELECT name, level_count AS "r2_4"
FROM  count_view_v
WHERE level = 'r2_4';

CREATE VIEW r4 AS
SELECT name, level_count AS "r4_6"
FROM  count_view_v
WHERE level = 'r4_6';

CREATE VIEW r6 AS
SELECT name, level_count AS "r6_8"
FROM  count_view_v
WHERE level = 'r6_8';

CREATE VIEW r8 AS
SELECT name, level_count AS "r8_10"
FROM  count_view_v
WHERE level = 'r8_10';

INSERT INTO q4
SELECT c.name, r0_2, r2_4, r4_6, r6_8, r8_10 FROM
country c NATURAL FULL JOIN 
(r0 NATURAL FULL JOIN r2 NATURAL FULL JOIN r4 
	NATURAL FULL JOIN r6 NATURAL FULL JOIN r8);



