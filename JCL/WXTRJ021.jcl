//WXTRJ021 JOB (640WXTR0100WXTRJ021,W100),'RTN WXTRV2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WXTR    EXEC WXTRP021                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ021                                         
