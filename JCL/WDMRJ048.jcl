//WDMRJ048 JOB (640W0030200WDMRJ048,W100),'RTN WDMRV2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJESD                                                             
/*ROUTE PRINT NJOVC                                                             
//*                                                                             
//WDMR    EXEC WDMRP048                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ048                                         
