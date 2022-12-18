/*데이터불러오기*/

proc import file = "C:\Users\jmjwj\Documents\UOS\3학년 1학기\다변량 통계학\world-happiness-report-2021.csv"
dbms = csv out = whr2021;
run;

/*데이터전처리*/

data whr_preprocessed;
set whr2021;
drop Ladder_score_in_Dystopia -- Dystopoia___residual;
run;

/*주성분분석 전 데이터표준화*/
proc standard data = whr_preprocessed out  = whr_std mean = 0 std= 1;
var Logged_GDP_per_capita -- Perceptions_of_corruption;
run;

/*주성분분석*/
proc princomp data = whr_std out = pca covariance;
var Logged_GDP_per_capita -- Perceptions_of_corruption;
run;

/*분석결과 : 4개의 주성분이 총 변동의 93%이상을 설명함.*/
/*해석*/
/*주성분 1  : 부유하고 자유로운 국가 => 선진국인가?*/
/*주성분 2: 유대감이 있는 국가*/
/*주성분 3: 사회적 유대감이 강하며 부패한국가인가*/
	/*주성분 4: 자유롭지 못한 독재국가*/

/*주성분 바탕으로 한 행복도 별 판별분석*/

/*1. 상위 25개국과 하위 25개국을 나누어 class 부여한다.*/

proc standard data = pca out = pca_std mean = 0 std = 1;
var Prin1--Prin6;
run;

data pca_ranked;
set pca_std;
row_number = _N_;
run;

data pca_classed;
set pca_ranked;
if row_number <= 25 then happy = 1; 
if row_number >= 125 then happy = 0; 
if row_number>=26 and row_number <125 then happy = .; 
run;

data pca_classed_for_discrim;
set pca_classed;
where happy = 1 or happy = 0;
run;

data pca_classed_for_test;
set pca_classed;
where happy = .;
run;


/*2.판별분석*/
proc discrim data = pca_classed_for_discrim simple listerr pool = yes;
class  happy;
var Prin1--Prin4;
priors prop;
run;

/*판별이 잘 된다.*/

/*3. test data로 판별분석*/
proc discrim data = pca_classed_for_discrim testdata = pca_classed_for_test testout = discrim_test_result testlist;
class happy;
var Prin1--Prin4;
priors prop;
run;

/*test결과도 구했다.*/

/*주성분 분석을 활용한 대륙별 판별분석*/
/*지역이 너무많다. 병합할 필요있음*/

data pca_std_region;
set pca_std;
if Regional_indicator = 'Western Europe' then region = 'Europe';
else if Regional_indicator = 'Central and Eastern Europe' then region = 'Europe';
else if Regional_indicator = 'Commonwealth of Independent' then region = 'Europe'; /*소련해체국가*/
else if Regional_indicator = 'East Asia' then region = 'Asia';
else if Regional_indicator = 'Latin America and Caribbean' then region = 'America';
else if Regional_indicator = 'Middle East and North Africa' then region = 'Africa'; /*중동국가는 아프리카로 편입*/
else if Regional_indicator = 'North America and ANZ' then region = 'America'; /*오세아니아는 아메리카로 편입*/
else if Regional_indicator = 'South Asia' then region = 'Asia';
else if Regional_indicator = 'Southeast Asia' then region = 'Asia';
else if Regional_indicator = 'Sub-Saharan Africa' then region = 'Africa';
run;

/*지역병합 후 판별분석 시행*/
proc discrim data = pca_std_region simple listerr pool = yes;
class region;
var Prin1-Prin4;
priors prop;

run;

/*지역병합 안하고 판별분석 시행*/
proc discrim data = pca_std_region simple listerr pool = yes;
class Regional_indicator;
var Prin1-Prin4;
priors prop;
run;

/*해석*/
/*분류가 잘 된 지역 : Central and Eastern Europe(17/12) , Latin America and Carribean(15/20), western Europe(21/17), sub-saharan africa(31/36)*/
/*경향을 띄는 지역 : middle east and north africa(9/17이 정확분류), north america and anz(모두 western Europe으로 판별), south-asia(5/7이 sub-saharan africa로 판별)*/
/*혼재지역 : Commonwealth of independent, eastasia (데이터 적음) , southeast asia -> 각각 할당할 것.*/ 

/*즉, 행복도를 기준으로 세계를 분류하자면, 
1. 중-동 유럽형 
2. 남아메리카형
3. 서유럽형
4. 사하라 이남 아프리카형
5. 북아프리카형
의 5개로 나눌 수 있음. */

proc sort data = pca_std_region;
by Regional_indicator;
run;

/*지역별로 병합*/
data pca_std_clustered;
set pca_std_region;
if Regional_indicator = 'Central and Eastern Europe' then cluster = 'ceEurope';
else if Regional_indicator = 'Latin America and Caribbean'  then cluster = 'sAmerica';
else if Regional_indicator = 'Western Europe' or Regional_indicator = 'North America and ANZ'  then cluster = 'wEurope';
else if Regional_indicator = 'Sub-Saharan Africa' or Regional_indicator = 'South Asia'  then cluster = 'sAfrica';
else if Regional_indicator = 'Middle East and North Africa'  then cluster = 'nafrica';
else if Regional_indicator = 'Commonwealth of Independent' or Regional_indicator = 'East Asia' or Regional_indicator = 'Southeast Asia'  then cluster= 'mixed';
drop region;
run;


/* 판별분석에 의한 clustering 이 의미를 가지는지에 대한 분산분석*/

/*1. 데이터의 정규성 검토 -> 어렵다*/
data x;
set pca_std_clustered;
keep Prin1--Prin4;
run;

proc iml;
use X;
read all into x;
mean = mean(x);
cov = cov(x);
md = (x - J(149,1)*mean) * inv(cov) * (x - J(149,1)*mean)`;
md2 =diag(md)*J(149,1);
call sort(md2, 1);
s = (T(1:149) - 0.375) / (149 + 0.25);
chisqQuant = quantile("ChiSquare", s, ncol(X));
call scatter(md2, chisqQuant);
run; 
/*데이터는 어느정도 다변량 정규성을 따른다*/

/*2. manova 시행*/
proc anova data = pca_std_clustered;
class cluster;
model Prin1 -- Prin4 = cluster;
manova h =cluster ;
run;
/*즉, 판별분석에 의한 분류는 유효하다.*/


/*각 군집별 outlier 탐색*/

/*흐텔링 T통계량 이용 -> 마할라노비스 거리와 유사형태*/

/*각 군집별 데이터셋 형성*/
data ceEurope;
set pca_std_clustered;
where cluster = 'ceEurope';
keep Prin1--Prin4;
run;

data mixed;
set pca_std_clustered;
where cluster = 'mixed';
keep Prin1--Prin4;
run;

data sAmerica;
set pca_std_clustered;
where cluster = 'sAmerica';
keep Prin1--Prin4;
run;

data wEurope;
set pca_std_clustered;
where cluster = 'wEurope';
keep Prin1--Prin4;
run;

data safrica;
set pca_std_clustered;
where cluster = 'sAfrica';
keep Prin1--Prin4;
run;

data nafrica;
set pca_std_clustered;
where cluster = 'nafrica';
keep Prin1--Prin4;
run;

/*proc iml 을 이용한 호텔링 통계량(마할라노비스 거리) 이용한 검정*/

/*1. 중부유럽*/

proc iml;
use ceEurope;
read all into X;
uvec = J(1, nrow(X))*X/nrow(X);
u = J(nrow(X), 1)*uvec;
cov = (X`*(I(nrow(X)) - J(nrow(X))/nrow(X))*X) / (nrow(X) -1);
T2 = diag((X-u)*inv(cov)*(X-u)`);
print(uvec);
run; 
/*자유도 1 카이제곱 분포의 95분위수 : 9.49*/
/*3, 6 데이터는 outlier*/

/*2. 북아프리카*/

proc iml;
use nafrica;
read all into X;
uvec = J(1, nrow(X))*X/nrow(X);
u = J(nrow(X), 1)*uvec;
cov = (X`*(I(nrow(X)) - J(nrow(X))/nrow(X))*X) / (nrow(X) -1);
T2 = diag((X-u)*inv(cov)*(X-u)`);
print(uvec);
run; 
/*11*/


/*3. 남아프리카*/

proc iml;
use safrica;
read all into X;
uvec = J(1, nrow(X))*X/nrow(X);
u = J(nrow(X), 1)*uvec;
cov = (X`*(I(nrow(X)) - J(nrow(X))/nrow(X))*X) / (nrow(X) -1);
T2 = diag((X-u)*inv(cov)*(X-u)`);
T2vec = T2*J(nrow(T2), 1);
print(uvec);
run; 
/*7,15,,42*/

/*3. 남아메리카*/

proc iml;
use samerica;
read all into X;
uvec = J(1, nrow(X))*X/nrow(X);
u = J(nrow(X), 1)*uvec;
cov = (X`*(I(nrow(X)) - J(nrow(X))/nrow(X))*X) / (nrow(X) -1);
T2 = diag((X-u)*inv(cov)*(X-u)`);
T2vec = T2*J(nrow(T2), 1);
print(uvec);
run; 
/*20*/

/*4. 서유럽*/

proc iml;
use weurope;
read all into X;
uvec = J(1, nrow(X))*X/nrow(X);
u = J(nrow(X), 1)*uvec;
cov = (X`*(I(nrow(X)) - J(nrow(X))/nrow(X))*X) / (nrow(X) -1);
T2 = diag((X-u)*inv(cov)*(X-u)`);
T2vec = T2*J(nrow(T2), 1);
print(uvec);
run; 

/*23,24,*/

/*이후  : 각 outlier 에 대한 개별 분석*/
/*mixed는  어떻게 처리?*/

