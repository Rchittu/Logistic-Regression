/**/
/**/
/**/
/*Task*/
/*A national veterans' organization wants to better target its solicitations for donation. By soliciting only the most likely donors, the organization can spend less money on solicitation efforts and more money on charitable concerns. Throughout the practices in this course, you'll apply your predictive modeling skills to this project.*/
/**/
/*The original sample of the veterans' organization's entire solicitations database, pmlr.pva_raw_data, needs further modification before it can be used for modeling. In this practice, you create and explore the pmlr.pva data set.*/
/**/
/*Note: Make sure you set up access to the course files.*/
/*Copy and paste the following DATA step into the editor in your SAS software:*/
/*data pmlr.pva(drop=control_number */
/*                   MONTHS_SINCE_LAST_PROM_RESP */
/*                   FILE_AVG_GIFT */
/*                   FILE_CARD_GIFT);*/
/*   set pmlr.pva_raw_data;*/
/*   STATUS_FL=RECENCY_STATUS_96NK in("F","L");*/
/*   STATUS_ES=RECENCY_STATUS_96NK in("E","S");*/
/*   home01=(HOME_OWNER="H");*/
/*   nses1=(SES="1");*/
/*   nses3=(SES="3");*/
/*   nses4=(SES="4");*/
/*   nses_=(SES="?");*/
/*   nurbr=(URBANICITY="R");*/
/*   nurbu=(URBANICITY="U");*/
/*   nurbs=(URBANICITY="S");*/
/*   nurbt=(URBANICITY="T");*/
/*   nurb_=(URBANICITY="?");*/
/*run;*/
/*Notice that this DATA step code drops several of the variables from pmlr.pva_raw_data and creates dummy variables for four existing variables. The dummy variables are used in a later practice.*/
/**/
/*Submit the code and check the log to verify that the pmlr.pva data set was created. How many observations and variables are in pmlr.pva?*/
/**/
/**/
/*To examine the contents of pmlr.pva, write a PROC CONTENTS step, submit it, and review the results. How many character variables are in the data set?*/
/**/
/**/
/*Write a PROC MEANS step that generates the following descriptive statistics for the numeric variables in the pmlr.pva data set: mean, number of missing values, maximum value, and minimum value. To specify only the numeric variables in the input data set, use the special SAS name list _NUMERIC_ in the VAR statement.*/
/**/
/*Submit the code for this step, view the results, and answer the following questions:*/
/**/
/*Is the proportion of events in the sample equal to the proportion of events in the population? (Hint: To find the proportion of events in the population, read the description of pmlr.pva_raw_data.)*/
/**/
/*What is the average number of months since the last gift to the organization?*/
/**/
/*How many numeric variables have missing values?*/
/**/
/**/
/*Write a PROC FREQ step that generates frequency tables for the character variables in the pmlr.pva data set. To specify only the character variables in the input data set, add the special SAS name list _CHARACTER_ to the TABLES statement. To display the Number of Variable Levels table for each variable specified in the TABLES statement, include the NLEVELS option in the PROC FREQ statement. */
/**/
/*Submit the code for this step, view the results, and answer the following questions:*/
/**/
/*Which character variable has the highest number of levels?*/
/**/
/*How many dummy variables would need to be created for the character variable that has the highest number of levels?*/


data pmlr.pva(drop=control_number 
                   MONTHS_SINCE_LAST_PROM_RESP 
                   FILE_AVG_GIFT 
                   FILE_CARD_GIFT);
   set pmlr.pva_raw_data; 

   STATUS_FL=RECENCY_STATUS_96NK in("F","L");
   STATUS_ES=RECENCY_STATUS_96NK in("E","S");
   home01=(HOME_OWNER="H");
   nses1=(SES="1");
   nses3=(SES="3");
   nses4=(SES="4");
   nses_=(SES="?");
   nurbr=(URBANICITY="R");
   nurbu=(URBANICITY="U");
   nurbs=(URBANICITY="S");
   nurbt=(URBANICITY="T");
   nurb_=(URBANICITY="?");
run ; 


proc contents data = pmlr.pva ; run; 


proc means data=pmlr.pva mean nmiss max min; 

   var _numeric_;

run; 


/*Write a PROC FREQ step that generates frequency tables for the character variables in the pmlr.pva data set.
To specify only the character variables in the input data set, add the special SAS name list _CHARACTER_ to the TABLES statement.
To display the Number of Variable Levels table for each variable specified in the TABLES statement, include the NLEVELS option in the 
PROC FREQ statement. */
/**/
/*Submit the code for this step, view the results, and answer the following questions:*/
/**/
/*Which character variable has the highest number of levels?*/
/**/
/*How many dummy variables would need to be created for the character variable that has the highest number of levels?*/





proc freq data = pmlr.pva NLEVELS ; 

	tables _CHARACTER_ ; 

run;  

/**/
/*Use PROC SURVEYSELECT to split the pmlr.pva data set into two data sets: a training data set named pmlr.pva_train 
and a validation data set named pmlr.pva_valid. Use 50% of the data for each data set role. Stratify on the target
variable, use a seed of 27513
, and use the STRATUMSEED=RESTORE option. Submit the code and check the log.*/

proc sort data = pmlr.pva out = pmlr.pva_sorted ;

	by Target_B ; 

run;  
/**/
/*Creating a dataset work.pva_sample which contain  new variable selected variable*/

proc surveyselect data = pmlr.pva_sorted  samprate = 0.5 out = work.pva_sample seed = 44444 outall ; 

	strata Target_B ; 

run;
/*	*/
/*Checking the stratified sampling*/

proc freq data = work.pva_sample ; 
	
	table Target_B * selected ; 

run ;

/**/
/*Creating seperate data sets pmlr.pva_train  and pmlr.pva_valid */  
/**/
/*" Out = " is used to create new datasets ;*/

data pmlr.pva_train pmlr.pva_valid ; 
	
	set work.pva_sample ; 

	if selected then output  pmlr.pva_train ;

		else output  pmlr.pva_valid ; 

run; 
/**/
/*Create a global macro variable named ex_pi1 that stores p1, the proportion of responders in the population. */
/*Note: To find the proportion of responders in the population, review the pva_raw_data data set description.*/

/**/
/*IT was given in the description that the proportion of events is 0.05; */

options mlogic symbolgen mprint mcompilenote = all;


%let ex_pi1 = 0.05 ; 





proc print data = pmlr.pva_train(obs = 10) ; 
var target_b  Pep_Star Recent_Avg_Gift_Amt Frequency_Status_97NK ; 
run; 



/*Add a PROC LOGISTIC step that does the following:*/
/**/
/*fits a logistic regression model with Target_B as the target variable, and Pep_Star ), */
/*Recent_Avg_Gift_Amt, and Frequency_Status_97NK as the input variables*/
/**/
/*models the probability that the target variable equals 1, and requests 95% profile likelihood */
/*confidence intervals for the odds ratio*/
/**/
/*uses the SCORE statement to create a temporary data set called scopva_train, */
/*which contains the data from the pva_train data set and the predicted probability of the event, */
/*correcting for oversampling*/
/**/
/*creates effect plots with confidence bands for the three input variables*/
/**/
/*creates effect plots of Recent_Avg_Gift_Amt by Pep_Star and Frequency_Status_97NK by PEP_STAR*/
/**/
/*creates an odds ratio plot with a horizontal orientation and displays the statistics*/
/**/
/*uses the ONLY global plot option to suppress the default plot*/


proc logistic data = pmlr.pva_train  
						plots(maxpoints=none only) = (   effect (clband) oddsratio (type =horizontalstat)    ) ; 

	class pep_star (param = ref ref = '0') ; 

	model target_b (event = '1')=  Pep_Star Recent_Avg_Gift_Amt Frequency_Status_97NK /    clodds = pl  ; 

	score data = pmlr.pva_train out = pmlr.scopva_train priorevent=&ex_pi1 ; 

	effectplot slicefit (sliceby =Pep_Star   x =Recent_Avg_Gift_Amt  )/  noobs ;  

	effectplot slicefit (sliceby =Pep_Star   x =Frequency_Status_97NK   )/ noobs ;

run; 


proc logistic data=pmlr.pva_train plots(only)=
              (effect(clband x=(pep_star recent_avg_gift_amt
              frequency_status_97nk)) oddsratio (type=horizontalstat));
   class pep_star (param=ref ref='0');
   model target_b(event='1')=pep_star recent_avg_gift_amt
                  frequency_status_97nk / clodds=pl;
   effectplot slicefit(sliceby=pep_star x=recent_avg_gift_amt) / noobs; 
   effectplot slicefit(sliceby=pep_star x=frequency_status_97nk) / noobs; 
   score data=pmlr.pva_train out=work.scopva_train priorevent=&ex_pi1;
run;


/**/
/*Missing Values */ 

/*Write a DATA step that creates missing value indicators for the following inputs in the pmlr.pva_train data set: */
/*Donor_Age, Income_Group, and Wealth_Rating. Also add a cumulative count of the missing values. Name the output data set pmlr.pva_train_mi.*/

%let intervals_i = Mi_Donor_Age Mi_Income_Group Mi_Wealth_Rating ; 
%let intervals = Donor_Age Income_Group Wealth_Rating ; 



data pmlr.pva_train_mi ; 
	
	set pmlr.pva_train ;  

	array mi_i { *} Mi_Donor_Age Mi_Income_Group Mi_Wealth_Rating ;

	array mi {*} Donor_Age Income_Group Wealth_Rating ; 

	do i = 1 to dim(mi) ; 
		mi_i(i) = ( mi(i) = . ) ;
	end; 

run;



proc print data =  pmlr.pva_train_mi ; 
var &intervals &intervals_i ; 
run; 
/**/
/*To group the values of the variables Recent_Response_Prop and Recent_Avg_Gift_Amt into three groups each, paste the following*/
/*PROC RANK code into the editor. Note that this code creates an output data set named work.pva_train_rank.*/
/**/

proc rank data=pmlr.pva_train_mi out=work.pva_train_rank groups=3;
   var recent_response_prop recent_avg_gift_amt;
   ranks grp_resp grp_amt;
run;
/**/
/*Sort the work.pva_train_rank data set by Grp_Resp and Grp_Amt. Name the output data set work.pva_train_rank_sort.*/
/**/
/*Submit the code and check the log to verify that the code ran without errors. */


proc sort data = work.pva_train_rank out = work.pva_train_rank_sort ; 
	by grp_resp grp_amt; 
run; 

/*To impute missing values in the work.pva_train_rank_sort data set for each BY group and create an output data set named*/
/*pmlr.pva_train_imputed, add a PROC STDIZE step with a BY statement. */


/* Cluster imputation: The data is sorted by the required variables and use proc stdize to replace the missing variables with the by group variables
*/ 

proc stdize data = work.pva_train_rank_sort reponly method = median out = pmlr.pva_train_imputed ;
	by grp_resp grp_amt ; 
	var &intervals ; 
run;  



/*Use PROC MEANS to determine the values that were used to replace the missing values in the pmlr.pva_train_imputed data set.*/
/*Add OPTIONS statements to display variable names instead of labels in the output from PROC MEANS (using the NOLABEL option) */
/*and then to reset the display of labels. Submit the code and look at the results.*/
/**/
/*For Grp_Resp=0 and Grp_Amt=0, what value replaced the missing value of Donor_Age?*/


options nolabel;
proc means data=pmlr.pva_train_imputed median;
   class grp_resp grp_amt;
   var DONOR_AGE INCOME_GROUP WEALTH_RATING;
run;
options label;


/**/
/*Practice: Computing the Smoothed Weight of Evidence*/
/**/

/*Write a PROC SQL step to compute the proportion of events in the pmlr.pva_train_imputed data set. */
/**/


proc sql ; 
	select mean(target_b) into : rho1 from pmlr.pva_train_imputed ; 
run;quit ;


/*Add a PROC MEANS step to calculate the response rate and frequency in each of the levels of Cluster_Code.*/
/**/
/*Submit the code and check the log to verify that the code ran without errors. */

proc means data =  pmlr.pva_train_imputed  sum nway noprint; 
	class cluster_code ; 
	var target_b ; 
	output out = work.counts sum = events ;
run; 


/*Create a SAS program file with the score code that computes the smoothed weight of evidence values. */
/*Use the value 24 for c and assign the overall logit to any observation with an undefined Cluster_Code. */
/*Define a new variable named Cluster_Swoe for the smoothed weight of evidence values. */
/**/
/*Add a DATA step to put the new assignments into a data set named pmlr.pva_train_imputed_swoe. */
/**/
/*Add a PROC PRINT step that helps you answer the following question:*/
/*What is the value of the smoothed weight of evidence for cluster code 01? */
/**/
/*Submit the code and look at the results. */

filename clswoe "D:\SAS_Buss_Analyst\swe.txt" ;



data _null_;
   file clswoe;
   set work.counts end=last;
      logit=log((events + &rho1_ex*24)/
            (_FREQ_ - events + (1-&rho1_ex)*24));
   if _n_=1 then put "select (cluster_code);" ;
   put "  when ('" cluster_code +(-1) "') 
       cluster_swoe=" logit ";" ;
   if last then do;
      logit=log(&rho1_ex/(1-&rho1_ex));
      put "  otherwise cluster_swoe=" logit ";" / "end;";
   end;
run;

data pmlr.pva_train_imputed_swoe;
   set pmlr.pva_train_imputed;
   %include clswoe;
run;

title;

proc print data=pmlr.pva_train_imputed_swoe(obs=1);
   where cluster_code = "01";
   var cluster_code cluster_swoe;
run;
