//WDMRJ046 JOB (640W0030200WDMRJ046,W100),'RTN WDMRV2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ   NJEVC                                                           
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDMR    EXEC WDMRP046                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ046                                         
