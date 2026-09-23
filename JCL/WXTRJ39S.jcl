//WXTRJ39S JOB (640W0001000WXTRJ39S,W100),'RTN WXTRS2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP39S                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ39S                                         
