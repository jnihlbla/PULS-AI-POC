//W011J273 JOB (640W0110100W011J273,W100),'RTN W100V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W011    EXEC W011P073,                                                        
//             INDUT=W011.QASE                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J273                                         
