//WXTRJ09C JOB (640W0001000WXTRJ09C,W100),'RTN WXTRD2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP09C,                                                        
//             INDIN=WXTR.WXTRD2,                                               
//             INDUT=WXTR.WXTRD2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ09C                                         
