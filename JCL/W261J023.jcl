//W261J023 JOB (640W2610100W261J023,W100),'RTN W261V5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W261    EXEC W261P023                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W261J023                                         
