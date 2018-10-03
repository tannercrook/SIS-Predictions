SELECT 
  p.studentNumber AS studentNumber
, ISNULL(g.onTimeGradYr, YEAR(g.diplomaDate)) AS schedGradYear 
, CASE 
	WHEN g.diplomaDate IS NOT NULL THEN 1
	ELSE 0
  END AS graduated 
, YEAR(g.diplomaDate) AS gradYear
, gs.number AS gradSchool 
, ISNULL(i.homePrimaryLanguage, 'eng') AS homeLanguage
FROM Person p 
INNER JOIN [Identity] i 
ON p.currentIdentityID = i.identityID
LEFT JOIN Graduation g 
ON p.personID = g.personID
LEFT JOIN School gs 
ON g.schoolID = gs.schoolID
WHERE g.onTimeGradYr <= 2018;
