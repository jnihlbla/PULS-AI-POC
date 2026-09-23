//WXTRJ490 JOB (640W0001000WXTRJ490,W100),'RTN WXTRS1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP090,                                                        
//             INDIN=W011.W011S1,                                               
//             INDUT=WXTR.WXTRS1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ490                                         
