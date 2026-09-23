//WDMRJ316 JOB (640W0030200WDMRJ316,W100),'RTN WDMRV3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJEVC                                                             
/*ROUTE PRINT NJOVC                                                             
//WDMR    EXEC WDMRP316                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ316                                         
