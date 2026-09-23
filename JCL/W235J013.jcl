//W235J013 JOB (640W2350100W235J013,W100),'RTN W235B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W235    EXEC W235P013                                                         
//*                                                                             
//MAIL  EXEC WMAILSND                                                           
)SEND                                                                           
TITLE CALL OFF LIST FROM SCREEN 2423                                            
TO  &MAIL                                                                       
ATTACH W235.W235B3.W2351301(+1)  W23513.XLS TEXT                                
MAIL                                                                            
 CALL OFF LIST                                                                  
 ORDERED FROM SCREEN 2423                                                       
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W235J013                                         
