//W212J012 JOB (640W2120100W212J012,W100),'RTN W212S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W212    EXEC W212P012                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W212J012                                         
