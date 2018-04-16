SET SEARCH_PATH to parlgov;

CREATE TABLE q5(
	countryName VARCHAR(50),
	partyName VARCHAR(100),
	partyFamily VARCHAR(30),
	stateMarket REAL
);

-- cabinet in past 20 years
CREATE VIEW cab_in_past20 AS
SELECT id,country_id
FROM cabinet 
WHERE start_date >= '1996.01.01' AND start_date < '2017.01.01';

-- cabinet parties in past 20 years
CREATE VIEW p_in_past20 AS
SELECT DISTINCT c20.country_id, cp.party_id
FROM cab_in_past20 c20, cabinet_party cp
WHERE cp.cabinet_id = c20.id;

-- all not qualified combination
CREATE VIEW not_qualified AS
(SELECT c20.country_id,c20.id, p20.party_id
FROM cab_in_past20 c20, p_in_past20 p20
WHERE c20.country_id = p20.country_id)
EXCEPT
(SELECT c20.country_id, c20.id, cp.party_id
FROM cab_in_past20 c20, cabinet_party cp
WHERE cp.cabinet_id = c20.id);

CREATE VIEW qualified AS
SELECT country_id,party_id 
FROM p_in_past20
WHERE (country_id,party_id) not in(
	SELECT country_id,party_id FROM not_qualified
);

INSERT INTO q5 
SELECT 
c.name, p.name, pf.family,
avg(pp.state_market)
FROM 
(qualified q LEFT JOIN party_family pf ON q.party_id = pf.party_id)
LEFT JOIN party_position pp ON q.party_id = pp.party_id,
cabinet cab, country c, party p
WHERE
cab.start_date >= '1996.01.01' AND cab.start_date < '2017.01.01' AND
q.party_id = p.id AND q.country_id = c.id 
GROUP BY c.name, p.name,pf.family;


