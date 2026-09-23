//W215J009 JOB (640W2150100W215J009,W100),'RTN W215D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W215    EXEC W215P009                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W215J009                                         
