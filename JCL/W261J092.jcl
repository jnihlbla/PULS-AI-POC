//W261J092 JOB (640W2610100W261J092,W100),'RTN W261V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W261    EXEC W261P092                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W261J092                                         
