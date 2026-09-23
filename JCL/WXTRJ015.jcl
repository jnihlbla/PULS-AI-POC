//WXTRJ015 JOB (640W0001000WXTRJ015,W100),'RTN WXTRV2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP015                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ015                                         
