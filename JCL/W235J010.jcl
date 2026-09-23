//W235J010 JOB (640W2350100W235J010,W100),'RTN W235B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W235    EXEC W235P010                                                         
//*                                                                             
//MAIL  EXEC WMAILSND                                                           
)SEND                                                                           
TITLE LEV.BESK.LISTA 2323                                                       
TO  &MAIL                                                                       
ATTACH W235.W235B2.W23511(+1)  W23511.XLS TEXT                                  
MAIL                                                                            
 LEVBESK,                                                                       
 ORDERED FROM SCREEN 2323                                                       
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W235J010                                         
