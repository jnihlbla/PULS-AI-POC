//WDMRJ416 JOB (640W0030200WDMRJ416,W100),'RTN WDMRV4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJEVC                                                             
/*ROUTE PRINT NJOVC                                                             
//WDMR    EXEC WDMRP416                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ416                                         
