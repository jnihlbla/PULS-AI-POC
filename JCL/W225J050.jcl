//W225J050 JOB (640W2250100W225J050,W100),'RTN W225V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W225    EXEC W225P050                                                         
//SORTWK01 DD  SPACE=(4000,(20000,10000),,,ROUND)                               
//SORTWK02 DD  SPACE=(4000,(20000,10000),,,ROUND)                               
//SORTWK03 DD  SPACE=(4000,(20000,10000),,,ROUND)                               
//SORTWK04 DD  SPACE=(4000,(20000,10000),,,ROUND)                               
//SORTWK05 DD  SPACE=(4000,(20000,10000),,,ROUND)                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W225J050                                         
