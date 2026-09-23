//W463J05C JOB (650W4630100W463J05C,W100),'RTN W463S9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//*                                                                             
//W463     EXEC W463P05C                                                        
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W463.W463S9.W46354E                                             
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP" AND FOLLOWED BY:                                             
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPDDGSVIKTTFEL                                                
//*    RCD 2: " ¤DAPBP3EA"  WHERE BP3EA IS IDLEVNR                              
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W463.W463S9.W4635E(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W463.W463S9.W4635E(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J05C                                         
