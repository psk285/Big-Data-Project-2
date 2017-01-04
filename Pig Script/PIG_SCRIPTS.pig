A = LOAD './pig/main_data.csv' using PigStorage(',') AS (country:chararray, years:int, students:int, gdp:long, 
expenditure:float);
B = GROUP A BY country;
C = FOREACH B GENERATE group as country, COUNT(A);
E = JOIN A by country, C by country;
F = FOREACH E GENERATE $0 as country,$1 as year,$2 as students,$3 as gdp, $4 as expenditure, $7 as counter;
G = FILTER F BY (counter > 2);
H = ORDER G BY country;;
STORE H INTO './PigScript_Result';

