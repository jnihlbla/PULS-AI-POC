//W463J098 JOB (650W4630100W463J098,W100),'RTN W463D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*                                                                             
//W463     EXEC W463P098                                                        
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W463.W463V2.W4639L                                              
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP" AND FOLLOWED BY:                                             
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPAWAITING-DDGS                                               
//*    RCD 2: " ¤DAPBP3EAW46398"  WHERE BP3EA IS IDLEVNR                        
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W463.W463D1.W4639N(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W463.W463D1.W4639N(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J098                                         
