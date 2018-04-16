SET SEARCH_PATH to parlgov;

SELECT * 
FROM q2
ORDER BY 
countryName ASC,
wonelections ASC,
partyName DESC;