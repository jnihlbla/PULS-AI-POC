//W114J026 JOB (640W1140100W114J026,W100),'RTN W114S9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W114    EXEC W114P026                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114J026                                         
