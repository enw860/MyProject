SET SEARCH_PATH to parlgov;

-- drop pre exist q1 and related view and tables
DROP TABLE IF EXISTS q1 CASCADE;
DROP VIEW vote_sum, remove_null_votes, votes_rate, election_in_20_years;

CREATE TABLE q1(
	year INT,
	countryName VARCHAR(50),
	voteRange VARCHAR(10),
	partyName VARCHAR(20)
);

-- sum up all elections votes, AND skip those elections
-- that have votes greater that valid_votes 
CREATE VIEW vote_sum AS
SELECT er.election_id
FROM election e, election_result er 
WHERE er.election_id = e.id AND e.votes_valid IS NOT NULL
GROUP BY er.election_id, e.votes_valid
HAVING sum(er.votes) <= e.votes_valid;

-- remove all parties that have empty votes
CREATE VIEW remove_null_votes AS
SELECT er.election_id, er.party_id, er.votes
FROM vote_sum vs NATURAL JOIN election_result er
WHERE er.votes IS NOT NULL;

-- calculate precentage of 
-- each parties' votes in each election
CREATE VIEW votes_rate AS
SELECT 
e.e_date, e.country_id, rnv.election_id, rnv.party_id, 
(100.0*votes/votes_valid) AS "rate"
FROM election e JOIN remove_null_votes rnv ON e.id = rnv.election_id;

-- append country name AND partyname to all qualified parties
CREATE VIEW election_in_20_years AS
SELECT c.name,p.name_short,
date_part('year',vr.e_date) AS "year", 
avg(vr.rate) AS "rate"
FROM votes_rate vr, country c , party p
WHERE 
vr.e_date >= '1996.01.01' AND vr.e_date < '2017.01.01' AND
vr.country_id = c.id AND vr.party_id = p.id
GROUP BY 
date_part('year',vr.e_date), c.name, p.name_short;

INSERT INTO q1
SELECT 
year,name,
CASE
WHEN ((rate>0)AND(rate<=5)) THEN '(0-5]'
WHEN ((rate>5)AND(rate<=10)) THEN '(5-10]'
WHEN ((rate>10)AND(rate<=20)) THEN '(10-20]'
WHEN ((rate>20)AND(rate<=30)) THEN '(20-30]'
WHEN ((rate>30)AND(rate<=40)) THEN '(30-40]'
WHEN ((rate>40)AND(rate<=100)) THEN '(40-100]'
END AS "level_rate",
name_short
FROM election_in_20_years;



