//W463J079 JOB (670W4630100W463J079,W100),'RTN W463D2',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* DELAY THE EXECUTION TO AVOID ABEND GG                                       
//WAIT    EXEC WWAIT,SECONDS=200                                                
//W463    EXEC W463P079                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J079                                         
