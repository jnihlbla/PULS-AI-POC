//WXTRJ49C JOB (640W0001000WXTRJ49C,W100),'RTN WXTRS1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP09C,                                                        
//             INDIN=WXTR.WXTRS1,                                               
//             INDUT=WXTR.WXTRS1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ49C                                         
