//W235J012 JOB (640W2350100W235J012,W100),'RTN W235B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W235    EXEC W235P012                                                         
//*                                                                             
//MAIL  EXEC WMAILSND                                                           
)SEND                                                                           
TITLE  DELIVERY PROMISES FROM SCREEN 2423                                       
TO     &MAIL                                                                    
ATTACH W235.W235B3.W2351201(+1)  W23512.XLS TEXT                                
MAIL                                                                            
 DELIVERY PROMISES                                                              
 ORDERED FROM SCREEN 2423                                                       
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W235J012                                         
