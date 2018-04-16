SET SEARCH_PATH to parlgov;

CREATE TABLE q6(
	countryName VARCHAR(50),
	cabinetId INT,
	startDate DATE,
	endDate DATE,
	pmParty VARCHAR(100)
);

INSERT INTO q6
SELECT c.name, cab.id, cab.start_date, MAX(cab2.start_date), p.name
FROM country c,
cabinet cab LEFT JOIN cabinet cab2 ON cab.previous_cabinet_id = cab2.id,
cabinet_party cp LEFT JOIN party p ON cp.party_id = p.id
WHERE cab.country_id = c.id AND cp.cabinet_id = cab.id AND cp.pm  = 'true'
GROUP BY c.name,cab.id, cab.start_date, p.name;

-- SELECT * FROM q6;



