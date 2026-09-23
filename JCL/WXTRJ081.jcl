//WXTRJ081 JOB (640W0001000WXTRJ081,W100),'RTN WXTRD1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP081                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ081                                         
