//W233J027 JOB (640W2330100W233J027,W100),'RTN W233V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W233    EXEC W233P027                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W233J027                                         
