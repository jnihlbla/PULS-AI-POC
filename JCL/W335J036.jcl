//W335J036 JOB (640W3350100W335J036,W100),'RTN W335V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W335    EXEC W335P036                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J036                                         
