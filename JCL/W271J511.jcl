//W271J511 JOB (670W2710100W271J511,W100),'RTN W271S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P511,                                                        
//             INDIN=W412.W412S2                                                
//*                                                                             
//*                                                                             
//MAIL  EXEC WMAILSND                                                           
)SEND                                                                           
TITLE ORDER UPLOAD DONE                                                         
TO   &MAILID                                                                    
MAIL                                                                            
 ORDER UPLOAD,                                                                  
 REFILL ORDERS UPDATED IN REVIEWED QUEUE                                        
)END                                                                            
//SOPEND  EXEC WSOPEND,PROCESS=W271J511                                         
