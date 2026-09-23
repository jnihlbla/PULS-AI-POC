//WXTRJ09T JOB (650W0001000WXTRJ09T,W100),'RTN WXTRD2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WXTR    EXEC WXTRP09T                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ09T                                         
