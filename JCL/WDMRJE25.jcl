//WDMRJE25 JOB (640W0030200WDMRJE25,W100),'RTN WDMRV6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WDMR    EXEC WDMRPE25                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJE25                                         
