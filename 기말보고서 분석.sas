/*�����ͺҷ�����*/

proc import file = "C:\Users\jmjwj\Documents\UOS\3�г� 1�б�\�ٺ��� �����\world-happiness-report-2021.csv"
dbms = csv out = whr2021;
run;

/*��������ó��*/

data whr_preprocessed;
set whr2021;
drop Ladder_score_in_Dystopia -- Dystopoia___residual;
run;

/*�ּ��км� �� ������ǥ��ȭ*/
proc standard data = whr_preprocessed out  = whr_std mean = 0 std= 1;
var Logged_GDP_per_capita -- Perceptions_of_corruption;
run;

/*�ּ��км�*/
proc princomp data = whr_std out = pca covariance;
var Logged_GDP_per_capita -- Perceptions_of_corruption;
run;

/*�м���� : 4���� �ּ����� �� ������ 93%�̻��� ������.*/
/*�ؼ�*/
/*�ּ��� 1  : �����ϰ� �����ο� ���� => �������ΰ�?*/
/*�ּ��� 2: ���밨�� �ִ� ����*/
/*�ּ��� 3: ��ȸ�� ���밨�� ���ϸ� �����ѱ����ΰ�*/
	/*�ּ��� 4: �������� ���� ���籹��*/

/*�ּ��� �������� �� �ູ�� �� �Ǻ��м�*/

/*1. ���� 25������ ���� 25������ ������ class �ο��Ѵ�.*/

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


/*2.�Ǻ��м�*/
proc discrim data = pca_classed_for_discrim simple listerr pool = yes;
class  happy;
var Prin1--Prin4;
priors prop;
run;

/*�Ǻ��� �� �ȴ�.*/

/*3. test data�� �Ǻ��м�*/
proc discrim data = pca_classed_for_discrim testdata = pca_classed_for_test testout = discrim_test_result testlist;
class happy;
var Prin1--Prin4;
priors prop;
run;

/*test����� ���ߴ�.*/

/*�ּ��� �м��� Ȱ���� ����� �Ǻ��м�*/
/*������ �ʹ�����. ������ �ʿ�����*/

data pca_std_region;
set pca_std;
if Regional_indicator = 'Western Europe' then region = 'Europe';
else if Regional_indicator = 'Central and Eastern Europe' then region = 'Europe';
else if Regional_indicator = 'Commonwealth of Independent' then region = 'Europe'; /*�ҷ���ü����*/
else if Regional_indicator = 'East Asia' then region = 'Asia';
else if Regional_indicator = 'Latin America and Caribbean' then region = 'America';
else if Regional_indicator = 'Middle East and North Africa' then region = 'Africa'; /*�ߵ������� ������ī�� ����*/
else if Regional_indicator = 'North America and ANZ' then region = 'America'; /*�����ƴϾƴ� �Ƹ޸�ī�� ����*/
else if Regional_indicator = 'South Asia' then region = 'Asia';
else if Regional_indicator = 'Southeast Asia' then region = 'Asia';
else if Regional_indicator = 'Sub-Saharan Africa' then region = 'Africa';
run;

/*�������� �� �Ǻ��м� ����*/
proc discrim data = pca_std_region simple listerr pool = yes;
class region;
var Prin1-Prin4;
priors prop;

run;

/*�������� ���ϰ� �Ǻ��м� ����*/
proc discrim data = pca_std_region simple listerr pool = yes;
class Regional_indicator;
var Prin1-Prin4;
priors prop;
run;

/*�ؼ�*/
/*�з��� �� �� ���� : Central and Eastern Europe(17/12) , Latin America and Carribean(15/20), western Europe(21/17), sub-saharan africa(31/36)*/
/*������ ��� ���� : middle east and north africa(9/17�� ��Ȯ�з�), north america and anz(��� western Europe���� �Ǻ�), south-asia(5/7�� sub-saharan africa�� �Ǻ�)*/
/*ȥ������ : Commonwealth of independent, eastasia (������ ����) , southeast asia -> ���� �Ҵ��� ��.*/ 

/*��, �ູ���� �������� ���踦 �з����ڸ�, 
1. ��-�� ������ 
2. ���Ƹ޸�ī��
3. ��������
4. ���϶� �̳� ������ī��
5. �Ͼ�����ī��
�� 5���� ���� �� ����. */

proc sort data = pca_std_region;
by Regional_indicator;
run;

/*�������� ����*/
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


/* �Ǻ��м��� ���� clustering �� �ǹ̸� ���������� ���� �л�м�*/

/*1. �������� ���Լ� ���� -> ��ƴ�*/
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
/*�����ʹ� ������� �ٺ��� ���Լ��� ������*/

/*2. manova ����*/
proc anova data = pca_std_clustered;
class cluster;
model Prin1 -- Prin4 = cluster;
manova h =cluster ;
run;
/*��, �Ǻ��м��� ���� �з��� ��ȿ�ϴ�.*/


/*�� ������ outlier Ž��*/

/*���ڸ� T��跮 �̿� -> ���Ҷ��� �Ÿ��� ��������*/

/*�� ������ �����ͼ� ����*/
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

/*proc iml �� �̿��� ȣ�ڸ� ��跮(���Ҷ��� �Ÿ�) �̿��� ����*/

/*1. �ߺ�����*/

proc iml;
use ceEurope;
read all into X;
uvec = J(1, nrow(X))*X/nrow(X);
u = J(nrow(X), 1)*uvec;
cov = (X`*(I(nrow(X)) - J(nrow(X))/nrow(X))*X) / (nrow(X) -1);
T2 = diag((X-u)*inv(cov)*(X-u)`);
print(uvec);
run; 
/*������ 1 ī������ ������ 95������ : 9.49*/
/*3, 6 �����ʹ� outlier*/

/*2. �Ͼ�����ī*/

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


/*3. ��������ī*/

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

/*3. ���Ƹ޸�ī*/

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

/*4. ������*/

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

/*����  : �� outlier �� ���� ���� �м�*/
/*mixed��  ��� ó��?*/

