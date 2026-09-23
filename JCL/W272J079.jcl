//W272J079 JOB (640W2720100W272J079,W100),'RTN W271V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W272    EXEC W272P079                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J079                                         
