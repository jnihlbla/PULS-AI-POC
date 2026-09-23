//W011J072 JOB (640W0110100W011J072,W100),'RTN W011D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W011    EXEC W011P072                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J072                                         
