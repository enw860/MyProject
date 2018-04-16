SET SEARCH_PATH to parlgov;

CREATE TABLE q2 (
	countryName VARCHAR(50),
	partyName	VARCHAR(100),
	partyFamily	VARCHAR(50),
	wonElections	INT,
	mostRecentlyWonElectionId INT,
	mostRecentlyWonElectionYear INT
);

CREATE VIEW winning_election_parties AS
SELECT c.id, er.election_id, er.party_id, er.votes, e.e_date
FROM election_result er, country c, election e
WHERE er.election_id = e.id AND e.country_id = c.id AND
er.votes IS NOT NULL AND
(er.election_id, er.votes) IN (
	SELECT election_id, max(votes)
	FROM election_result
	GROUP BY election_id
);

CREATE VIEW winning_count AS
SELECT id, party_id, count(*) AS "win_count", max(e_date) AS "recent_win_date"
FROM winning_election_parties
GROUP BY id, party_id;

CREATE VIEW winning_parties_recent_id AS
SELECT wc.id, wc.party_id, wc.win_count, wep.election_id, wc.recent_win_date
FROM winning_count wc, winning_election_parties wep
WHERE wc.id = wep.id AND 
wc.party_id = wep.party_id AND 
wc.recent_win_date = wep.e_date;

CREATE VIEW country_sum_win AS 
SELECT id, sum(win_count) AS "sum"
FROM winning_count
GROUP BY id;

CREATE VIEW country_avg_win AS
SELECT p.country_id AS "id", 1.0*csw.sum/COUNT(DISTINCT p.id) AS "average"
FROM country_sum_win csw,party p
WHERE csw.id = p.country_id
GROUP BY p.country_id,csw.sum;

CREATE VIEW party_win_more_than_avg AS
SELECT wpri.id, wpri.party_id, wpri.win_count, 
wpri.election_id,wpri.recent_win_date
FROM winning_parties_recent_id wpri, country_avg_win caw
WHERE wpri.id = caw.id AND wpri.win_count > 3*caw.average;

INSERT INTO q2
SELECT c.name, p.name, pf.family, pwmta.win_count,pwmta.election_id,
date_part('year',pwmta.recent_win_date) 
FROM party_win_more_than_avg pwmta, country c,
party p LEFT JOIN party_family pf ON p.id = pf.party_id
WHERE 
pwmta.id = c.id AND
pwmta.party_id = p.id;


