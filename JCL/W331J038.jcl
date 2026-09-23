//W331J038 JOB (640W3310100W331J038,W100),'RTN W331V5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W331    EXEC W331P038                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W331J038                                         
