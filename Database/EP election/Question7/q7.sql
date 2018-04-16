SET SEARCH_PATH to parlgov;

DROP TABLE IF EXISTS q7 CASCADE;
DROP VIEW country_election_count, head_allied_party, bi_dir_head_allied_party
qualified_head_paired, qualified_paired_expect_head, qualified_parties,
party_alliance_count;

CREATE TABLE q7(
	countryId INT,
	alliedPartyId1 INT,
	alliedPartyId2 INT
);

CREATE VIEW country_election_count AS
SELECT country_id, count(e.id)
FROM election e
GROUP BY country_id;

CREATE VIEW head_allied_party AS
SELECT er1.election_id,er1.party_id AS "a_1" ,er2.party_id AS "a_2"
FROM election_result er1, election_result er2
WHERE er1.alliance_id = er2.id AND
er1.election_id = er2.election_id;

CREATE VIEW bi_dir_head_allied_party AS
(SELECT election_id,a_1,a_2 FROM head_allied_party)
UNION ALL
(SELECT election_id,a_2,a_1 FROM head_allied_party);

CREATE VIEW qualified_head_paired AS
SELECT * FROM bi_dir_head_allied_party
WHERE a_1<a_2;

CREATE VIEW qualified_paired_expect_head AS
SELECT a1.election_id, a1.a_1, a2.a_1 AS "a_2"
FROM head_allied_party a1, head_allied_party a2
WHERE a1.election_id = a2.election_id AND 
a1.a_2 = a2.a_2 AND a1.a_1 < a2.a_1;

CREATE VIEW qualified_parties AS
(SELECT * FROM qualified_head_paired) 
UNION 
(SELECT * FROM qualified_paired_expect_head);

CREATE VIEW party_alliance_count AS
SELECT e.country_id,qp.a_1,qp.a_2, count(qp.election_id)
FROM qualified_parties qp, election e
WHERE e.id = qp.election_id
GROUP BY e.country_id,qp.a_1,qp.a_2;

INSERT INTO q7
SELECT pac.country_id, pac.a_1,pac.a_2
FROM party_alliance_count pac,country_election_count cec
WHERE 
cec.country_id = pac.country_id AND
pac.count >= cec.count*0.3;

