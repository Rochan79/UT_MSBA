CREATE TABLE student
( 
STUDID NUMBER(6) PRIMARY KEY,
STDFIRSTNAME varchar2(30) NOT NULL,
STDLASTNAME varchar2(30),
STREAM varchar2(30) CHECK (STREAM IN ('Accounting','Finance','IB','Marketing','MIS','Analytics'))
);

DESC STUDENT

CREATE TABLE academic
( 
STUDID NUMBER(6),
SECTION varchar2(1) CHECK (SECTION IN ('A','B')),
COURSENAME varchar2(30),
TOTALSCORE NUMBER(5,2) CHECK (TOTALSCORE BETWEEN 0.00 AND 100.00),
FOREIGN KEY (STUDID) REFERENCES student (STUDID)
);

DESC academic;

select * from academic;

--#2
--Display only the first and last names, and courses each student is enrolled in

SELECT T1.StdFirstName,T1.StdLastName,T2.CourseName
FROM student T1
LEFT JOIN academic T2
ON T1.STUDID=T2.STUDID;

--#3
--Which students are failing in which classes, where the failing grade is 40%?

SELECT T1.StdFirstName,T1.StdLastName,T2.CourseName,T2.TotalScore,T2.SECTION
FROM student T1
LEFT JOIN academic T2
ON T1.STUDID=T2.STUDID
WHERE TotalScore <= 40.00;

--#4
--Which students from the Analytics stream are failing?

SELECT T1.StdFirstName,T1.StdLastName,T2.CourseName,T2.TotalScore,T2.SECTION
FROM student T1
LEFT JOIN academic T2
ON T1.STUDID=T2.STUDID
WHERE TotalScore <= 40.00 and STREAM='Analytics';


--#5
--Now alter the table(s) by adding a Professor to each class being taught. Right now keep the professor name empty. Show the new table(s)

Alter table academic
Add Professor VARCHAR(30);

SELECT * FROM academic;




--#6 
--Change the student name ‘Marie Curie’ to ‘Pierre Curie’

UPDATE student
SET StdFirstName='Pierre'
WHERE StdFirstName='Marie' and StdLastName='Curie';

SELECT * FROM student;





--#7
--Display the full record for those students whose first name contains the regular 
--expression ‘ie’. For example, the word lied has the regular expression ‘ie’, while lai does not.

SELECT *
FROM student T1
LEFT JOIN academic T2
ON T1.STUDID=T2.STUDID
WHERE StdFirstName like '%ie%';






--#8
--Find all the students from the Analytics stream whose score is greater than the average of the Analytic stream students.

SELECT *
FROM student T1
LEFT JOIN academic T2
ON T1.STUDID=T2.STUDID
WHERE STREAM='Analytics'
and
t2.totalscore >
(
    SELECT avg(t2.totalscore)
    FROM student T1
    LEFT JOIN academic T2
    ON T1.STUDID=T2.STUDID
    WHERE STREAM='Analytics'
);




--#9
--Print the information from these columns StudID, StdFirstName, 
--StdLastName, TotalScore, CourseName, Section, Stream sorted on the last name of the students

SELECT T1.StudID,T1.StdFirstName,T1.StdLastName,T2.TotalScore,T2.CourseName,T2.SECTION,T1.Stream
FROM student T1
LEFT JOIN academic T2
ON T1.STUDID=T2.STUDID
ORDER BY T1.StdLastName;








--#10
--Find the student who received the highest score on each subject 
--(ignore the sections A and B for each subject to find the topper in each subject)

SELECT * FROM 
(SELECT T1.STUDID, T1.STDFIRSTNAME, T1.STDLASTNAME, T2.COURSENAME, T2.TOTALSCORE FROM STUDENT T1 LEFT JOIN ACADEMIC T2 ON T1.STUDID = T2.STUDID)  T1 right JOIN (

select  COURSENAME , MAX(TOTALSCORE) MAX_SCORE FROM ACADEMIC GROUP BY  COURSENAME
)
T2 ON T1.TOTALSCORE = T2.MAX_SCORE AND T1.COURSENAME=T2.COURSENAME;





