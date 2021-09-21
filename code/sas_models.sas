data Onfarm_practices_source;
infile cards dlm='09'x;
input treatment replicate farms;
datalines;









;
Proc glm data=Onfarm_practices_source plots=diagnostics;
class treatment replicate;
model farms = replicate Treatment  /ss3;
lsmeans treatment / cl pdiff lines ;
run;
proc glimmix data= Onfarm_practices_source;
class treatment replicate;
model farms=treatment replicate ;
lsmeans treatment/pdiff lines adjust=tukey;
run;
