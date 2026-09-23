//W114J052 JOB (640W1140100W114J052,W100),'RTN W114D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
//      INCLUDE MEMBER=SYST1                                                    
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W114    EXEC W114P052                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114J052                                         
