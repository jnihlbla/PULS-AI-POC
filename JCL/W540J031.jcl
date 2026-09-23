//W540J031 JOB (670W5400100W540J031,W100),'RTN W540V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W540    EXEC W540P031                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W540J031                                         
