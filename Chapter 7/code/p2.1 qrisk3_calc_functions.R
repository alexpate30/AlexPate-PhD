### SET THE ROOT DIRECTORY

### SET THE ROOT DIRECTORY


#### THESE PROGRAMS ARE TAKEN FROM THE PACKAGE CREATED BY YAN LI:
#### Li Y, Sperrin M, van Staa T. R package "QRISK3": an unofficial research purposed 
#### implementation of ClinRisk's QRISK3 algorithm into R. F1000Research. 2019;8:2139. 


#### The function will take an input dataset (of the right format) and produce QRISK3 scores for each
#### observation in the dataset

library(dplyr)

test_q <- read.csv("data/CSV2019/test_q.csv", check.names=FALSE)
test_all <- rbind(test_q, test_q)
str(test_all)
# Metadata;
#   age=64;
#   gender=1; /*1:women 2:men*/
#   b_AF=1;  /*Atrial fibrillation?*/
#   b_atypicalantipsy=1; /*On atypical antipsychotic medication?*/
#   b_corticosteroids=1; /*Are you on regular steroid tablets?*/
#   b_impotence2 =1; /*A diagnosis of or treatment for erectile disfunction?*/
#   b_migraine=1; /*Do you have migraines?*/
#   b_ra=0; /*Rheumatoid arthritis?*/
#   b_renal=0; /*Chronic kidney disease (stage 3, 4 or 5)?*/
#   b_semi=0; /*Severe mental illness?*/
#   b_sle=0; /*Systemic lupus erythematosis (SLE)?*/
#   b_treatedhyp=1; /*On blood pressure treatment?*/
#   b_type1=1; /*Diabetes status: type 1*/
#   b_type2=0; /*Diabetes status: type 2*/
#   weight=80; (kg)
#   height=178; (cm)
#   ethrisk=1; /*risk of ethic group*/
#   1 = white or not stated
#   2 = indian
#   3 = pakistani
#   4 = bangladeshi
#   5 = other asian
#   6 = black caribbean
#   7 = black african
#   8 = chinese
#   9 = other
#   fh_cvd=1; /*Angina or heart attack in a 1st degree relative < 60?*/
#   rati=4; /*1 - 11, Cholesterol/HDL ratio:*/
#   sbp=180; /*Systolic blood pressure (mmHg)*/
#   sbps5=20; /*Standard deviation of at least two most recent systolic blood pressure readings (mmHg)*/
#   smoke_cat=1;
#   **This is differnt from original C program because index of SAS and R vector starts from 1;
#   /*     value='1' non-smoker*/
#   /*     value='2' >ex-smoker*/
#   /*     value='3' >light smoker (less than 10)*/
#   /*     value='4' >moderate smoker (10 to 19)*/
#   /*     value='5' >heavy smoker (20 or over)*/
#   surv=10; /*10 year risk*/
#   town=0; /*town deprivation score*/
#   bmi= weight / ((height/100)**2);

qrisk_calc_female <- function(data.in){

#Female 
survivor <- c(0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0.988876402378082,
              0,
              0,
              0,
              0,
              0)

#survivor[10]

Iethrisk <- c(0, # white
              0.2804031433299542500000000, # indian
              0.5629899414207539800000000, # pakistani
              0.2959000085111651600000000, # bangladeshi
              0.0727853798779825450000000, # other asian
              -0.1707213550885731700000000, # black caribbean
              -0.3937104331487497100000000, # black african
              -0.3263249528353027200000000, # chinese
              -0.1712705688324178400000000) # other

exp(Iethrisk)
#Iethrisk[1]

Ismoke <- c(	0,
              0.1338683378654626200000000,
              0.5620085801243853700000000,
              0.6674959337750254700000000,
              0.8494817764483084700000000)

#Ismoke[2]

#Calculate score
#female
fe_rst <- as.data.frame(within(data.in[data.in$gender==1, ], {
     #Applying the fractional polynomial transforms 
     #(which includes scaling)                      
       
     dage = age
     dage=dage/10
     age_1 = dage^(-2)
     age_2 = dage
     dbmi = bmi
     dbmi=dbmi/10
     bmi_1 = dbmi^(-2)
     bmi_2 = (dbmi^(-2))*log(dbmi)
     
     # Centring the continuous variables 
       
     age_1 = age_1 - 0.053274843841791
     age_2 = age_2 - 4.332503318786621
     bmi_1 = bmi_1 - 0.154946178197861
     bmi_2 = bmi_2 - 0.144462317228317
     rati = rati - 3.476326465606690
     sbp = sbp - 123.130012512207030
     sbps5 = sbps5 - 9.002537727355957
     town = town - 0.392308831214905
    
     # Start of Sum 
       a=0
     
     #The conditional sums 
       
       a =a+ Iethrisk[ethrisk]
     a =a+ Ismoke[smoke_cat]
     
     # Sum from continuous values 
       
       a =a+ age_1 * -8.1388109247726188000000000
     a =a+ age_2 * 0.7973337668969909800000000
     a =a+ bmi_1 * 0.2923609227546005200000000
     a =a+ bmi_2 * -4.1513300213837665000000000
     a =a+ rati * 0.1533803582080255400000000
     a =a+ sbp * 0.0131314884071034240000000
     a =a+ sbps5 * 0.0078894541014586095000000
     a =a+ town * 0.0772237905885901080000000
     
     # Sum from boolean values 
       
       a =a+ b_AF * 1.5923354969269663000000000
     a =a+ b_atypicalantipsy * 0.2523764207011555700000000
     a =a+ b_corticosteroids * 0.5952072530460185100000000
     a =a+ b_migraine * 0.3012672608703450000000000
     a =a+ b_ra * 0.2136480343518194200000000
     a =a+ b_renal * 0.6519456949384583300000000
     a =a+ b_semi * 0.1255530805882017800000000
     a =a+ b_sle * 0.7588093865426769300000000
     a =a+ b_treatedhyp * 0.5093159368342300400000000
     a =a+ b_type1 * 1.7267977510537347000000000
     a =a+ b_type2 * 1.0688773244615468000000000
     a =a+ fh_cvd * 0.4544531902089621300000000
     
     # Sum from interaction terms 
       
       a =a+ age_1 * (smoke_cat==2) * -4.7057161785851891000000000
     a =a+ age_1 * (smoke_cat==3) * -2.7430383403573337000000000
     a =a+ age_1 * (smoke_cat==4) * -0.8660808882939218200000000
     a =a+ age_1 * (smoke_cat==5) * 0.9024156236971064800000000
     a =a+ age_1 * b_AF * 19.9380348895465610000000000
     a =a+ age_1 * b_corticosteroids * -0.9840804523593628100000000
     a =a+ age_1 * b_migraine * 1.7634979587872999000000000
     a =a+ age_1 * b_renal * -3.5874047731694114000000000
     a =a+ age_1 * b_sle * 19.6903037386382920000000000
     a =a+ age_1* b_treatedhyp * 11.8728097339218120000000000
     a =a+ age_1 * b_type1 * -1.2444332714320747000000000
     a =a+ age_1 * b_type2 * 6.8652342000009599000000000
     a =a+ age_1 * bmi_1 * 23.8026234121417420000000000
     a =a+ age_1 * bmi_2 * -71.1849476920870070000000000
     a =a+ age_1 * fh_cvd * 0.9946780794043512700000000
     a =a+ age_1 * sbp * 0.0341318423386154850000000
     a =a+ age_1 * town * -1.0301180802035639000000000
     a =a+ age_2 * (smoke_cat==2) * -0.0755892446431930260000000
     a =a+ age_2 * (smoke_cat==3) * -0.1195119287486707400000000
     a =a+ age_2 * (smoke_cat==4) * -0.1036630639757192300000000
     a =a+ age_2 * (smoke_cat==5) * -0.1399185359171838900000000
     a =a+ age_2 * b_AF * -0.0761826510111625050000000
     a =a+ age_2 * b_corticosteroids * -0.1200536494674247200000000
     a =a+ age_2 * b_migraine * -0.0655869178986998590000000
     a =a+ age_2 * b_renal * -0.2268887308644250700000000
     a =a+ age_2 * b_sle * 0.0773479496790162730000000
     a =a+ age_2* b_treatedhyp * 0.0009685782358817443600000
     a =a+ age_2 * b_type1 * -0.2872406462448894900000000
     a =a+ age_2 * b_type2 * -0.0971122525906954890000000
     a =a+ age_2 * bmi_1 * 0.5236995893366442900000000
     a =a+ age_2 * bmi_2 * 0.0457441901223237590000000
     a =a+ age_2 * fh_cvd * -0.0768850516984230380000000
     a =a+ age_2 * sbp * -0.0015082501423272358000000
     a =a+ age_2 * town * -0.0315934146749623290000000
     
     # Calculate the score itself 
       score = 100.0 * (1 - (survivor[surv])^exp(a))
       lp = a
       score1= round(score,1)}))

return(fe_rst[,c("patid","score","lp")])}

#Male
#Attention: The three arrays were re-defined here. You must run them together.
#Female
qrisk_calc_male <- function(data.in){
  
survivor <- c(	0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0.977268040180206,
                0,
                0,
                0,
                0,
                0)
#survivor[10]

Iethrisk <- c(		0,
                0.2771924876030827900000000,
                0.4744636071493126800000000,
                0.5296172991968937100000000,
                0.0351001591862990170000000,
                -0.3580789966932791900000000,
                -0.4005648523216514000000000,
                -0.4152279288983017300000000,
                -0.2632134813474996700000000)
#Iethrisk[1]

Ismoke <- c(	0,
             0.1912822286338898300000000,
             0.5524158819264555200000000,
             0.6383505302750607200000000,
             0.7898381988185801900000000)

#Ismoke[2]

Ma_rst <- as.data.frame(within(data.in[data.in$gender==2, ], {
  # Applying the fractional polynomial transforms 
  # (which includes scaling)                      
    
    dage = age
    dage=dage/10
    age_1 = dage^(-1)
    age_2 = dage^(3)
    dbmi = bmi
    dbmi=dbmi/10
    bmi_2 = dbmi^(-2)*log(dbmi)
    bmi_1 = dbmi^(-2)
    
    # Centring the continuous variables 
      
      age_1 = age_1 - 0.234766781330109
      age_2 = age_2 - 77.284080505371094
      bmi_1 = bmi_1 - 0.149176135659218
      bmi_2 = bmi_2 - 0.141913309693336
      rati = rati - 4.300998687744141
      sbp = sbp - 128.571578979492190
      sbps5 = sbps5 - 8.756621360778809
      town = town - 0.526304900646210
      
      # Start of Sum 
        a=0
        
        # The conditional sums 
          
          a =a+ Iethrisk[ethrisk]
          a =a+ Ismoke[smoke_cat]
          
          # Sum from continuous values 
            
            a =a+ age_1 * -17.8397816660055750000000000
            a =a+ age_2 * 0.0022964880605765492000000
            a =a+ bmi_1 * 2.4562776660536358000000000
            a =a+ bmi_2 * -8.3011122314711354000000000
            a =a+ rati * 0.1734019685632711100000000
            a =a+ sbp * 0.0129101265425533050000000
            a =a+ sbps5 * 0.0102519142912904560000000
            a =a+ town * 0.0332682012772872950000000
          
            # Sum from boolean values 
              
              a =a+ b_AF * 0.8820923692805465700000000
              a =a+ b_atypicalantipsy * 0.1304687985517351300000000
              a =a+ b_corticosteroids * 0.4548539975044554300000000
              a =a+ b_impotence2 * 0.2225185908670538300000000
              a =a+ b_migraine * 0.2558417807415991300000000
              a =a+ b_ra * 0.2097065801395656700000000
              a =a+ b_renal * 0.7185326128827438400000000
              a =a+ b_semi * 0.1213303988204716400000000
              a =a+ b_sle * 0.4401572174457522000000000
              a =a+ b_treatedhyp * 0.5165987108269547400000000
              a =a+ b_type1 * 1.2343425521675175000000000
              a =a+ b_type2 * 0.8594207143093222100000000
              a =a+ fh_cvd * 0.5405546900939015600000000
              
              # Sum from interaction terms 
                
                a =a+ age_1 * (smoke_cat==2) * -0.2101113393351634600000000
                a =a+ age_1 * (smoke_cat==3) * 0.7526867644750319100000000
                a =a+ age_1 * (smoke_cat==4) * 0.9931588755640579100000000
                a =a+ age_1 * (smoke_cat==5) * 2.1331163414389076000000000
                a =a+ age_1 * b_AF * 3.4896675530623207000000000
                a =a+ age_1 * b_corticosteroids * 1.1708133653489108000000000
                a =a+ age_1 * b_impotence2 * -1.5064009857454310000000000
                a =a+ age_1 * b_migraine * 2.3491159871402441000000000
                a =a+ age_1 * b_renal * -0.5065671632722369400000000
                a =a+ age_1* b_treatedhyp * 6.5114581098532671000000000
                a =a+ age_1 * b_type1 * 5.3379864878006531000000000
                a =a+ age_1 * b_type2 * 3.6461817406221311000000000
                a =a+ age_1 * bmi_1 * 31.0049529560338860000000000
                a =a+ age_1 * bmi_2 * -111.2915718439164300000000000
                a =a+ age_1 * fh_cvd * 2.7808628508531887000000000
                a =a+ age_1 * sbp * 0.0188585244698658530000000
                a =a+ age_1 * town * -0.1007554870063731000000000
                a =a+ age_2 * (smoke_cat==2) * -0.0004985487027532612100000
                a =a+ age_2 * (smoke_cat==3) * -0.0007987563331738541400000
                a =a+ age_2 * (smoke_cat==4) * -0.0008370618426625129600000
                a =a+ age_2 * (smoke_cat==5) * -0.0007840031915563728900000
                a =a+ age_2 * b_AF * -0.0003499560834063604900000
                a =a+ age_2 * b_corticosteroids * -0.0002496045095297166000000
                a =a+ age_2 * b_impotence2 * -0.0011058218441227373000000
                a =a+ age_2 * b_migraine * 0.0001989644604147863100000
                a =a+ age_2 * b_renal * -0.0018325930166498813000000
                a =a+ age_2* b_treatedhyp * 0.0006383805310416501300000
                a =a+ age_2 * b_type1 * 0.0006409780808752897000000
                a =a+ age_2 * b_type2 * -0.0002469569558886831500000
                a =a+ age_2 * bmi_1 * 0.0050380102356322029000000
                a =a+ age_2 * bmi_2 * -0.0130744830025243190000000
                a =a+ age_2 * fh_cvd * -0.0002479180990739603700000
                a =a+ age_2 * sbp * -0.0000127187419158845700000
                a =a+ age_2 * town * -0.0000932996423232728880000
                
    # Calculate the score itself 
    score = 100.0 * (1 - (survivor[surv])^exp(a))
    lp = a
    score1= round(score,1)}))

return(Ma_rst[,c("patid","score","lp")])}

rm(list=setdiff(ls(),list("qrisk_calc_female","qrisk_calc_male")))
save.image("R_out_Vis2019/qrisk_calc_functions.RData")