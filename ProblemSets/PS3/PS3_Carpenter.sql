sqlite> .mode csv
sqlite> .import /home/ouecon022/DScourseS20/ProblemSets/PS3/FL_insurance_sample.csv d                                                                                         f


sqlite> SELECT *
   ...> FROM df
   ...> LIMIT 10;

sqlite> SELECT  DISTINCT county
   ...> FROM df
   ...> ORDER BY county;

sqlite> SELECT AVG(tiv_2012), AVG(tiv_2011)
   ...> FROM df;

sqlite> SELECT construction, COUNT(*)
   ...> FROM df
   ...> GROUP BY construction
   ...> ORDER BY COUNT(*) DESC;

