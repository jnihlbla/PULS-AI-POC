//W261J05A JOB (640W2610100W261J05A,W100),'RTN W261V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W261    EXEC W261P05A                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W261J05A                                         
