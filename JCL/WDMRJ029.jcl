//WDMRJ029 JOB (640W0030200WDMRJ029,W100),'RTN WDMRV6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
//      INCLUDE MEMBER=DESTN                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WDMR    EXEC WDMRP029                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ029                                         
