//W402UXDE JOB (640W4120100W402UXDE,W100),'RTN W412V9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//DELPDF  EXEC WOPNMVS                                                          
//SYSIN     DD DSN=W.PROD.CONSTANT(W402UXDE),DISP=SHR                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W402UXDE                                         
