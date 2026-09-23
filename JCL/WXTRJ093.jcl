//WXTRJ093 JOB (640W0001000WXTRJ093,W100),'RTN WXTRD2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP093                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ093                                         
