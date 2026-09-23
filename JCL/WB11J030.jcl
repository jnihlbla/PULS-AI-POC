//WB11J030 JOB (640WB010100WB11J030,W100),'RTN WB11D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ  NJEVC                                                            
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//DELPDF  EXEC WOPNMVS                                                          
//SYSIN     DD DSN=W.QASE.CONSTANT(WB11DELP),DISP=SHR                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WB11J030                                         
