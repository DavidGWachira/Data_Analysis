/*Task 1 - Import the dataset*/
proc import datafile="/home/u62138261/Exam2/Exam02DataForPost.csv"
     out=work.NBA 
     dbms=csv
     replace;
     getnames=yes;
     guessingrows=max;
run;

/*Task 2 - Multiple Linear Regression Model*/
ods noproctitle;
ods graphics / imagemap=on;

proc glmselect data=WORK.NBA outdesign(addinputvars)=Work.reg_design;
	class Season Conference / param=glm;
	model W=Age SOS ORtg Pace FTr '3PAr'n 'eFG%'n 'TOV%'n 'ORB%'n 'FT/FGA'n 
		'OppeFG%'n 'OppTOV%'n 'DRB%'n 'OppFT/FGA'n Season Conference / showpvalues 
		selection=none;
run;

proc reg data=Work.reg_design alpha=0.05 plots(only label)=(diagnostics 
		residuals rstudentbypredicted dffits dfbetas);
	where Season is not missing and Conference is not missing;
	ods select ParameterEstimates DiagnosticsPanel ResidualPlot 
		RStudentByPredicted DFFITSPlot DFBETASPanel;
	model W=&_GLSMOD / vif;
	run;
quit;

proc delete data=Work.reg_design;
run;
title "Influential Observations by RStudent Plot";
proc print data=Rstud;
run;

/*Task 3 - Regression Model using the stepwise selection procedure using Mallow’s C(p) as the selection criterion*/
ods noproctitle;
ods graphics / imagemap=on;

proc glmselect data=WORK.NBA outdesign(addinputvars)=Work.reg_design 
		plots=(criterionpanel);
	class Season Conference / param=glm;
	model W=Age SOS ORtg Pace FTr '3PAr'n 'eFG%'n 'TOV%'n 'ORB%'n 'FT/FGA'n 
		'OppeFG%'n 'OppTOV%'n 'DRB%'n 'OppFT/FGA'n Season Conference / showpvalues 
		selection=stepwise
    
   (select=cp choose=cp);
run;

proc reg data=Work.reg_design alpha=0.05 plots(only)=(diagnostics residuals);
	where Season is not missing and Conference is not missing;
	ods select ParameterEstimates DiagnosticsPanel ResidualPlot;
	model W=&_GLSMOD / vif;
	run;
quit;

proc delete data=Work.reg_design;
run;

