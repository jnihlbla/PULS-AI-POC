//WDMRJ035 JOB (640W0030200WDMRJ035,W100),'RTN WDMRV5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJESD                                                             
/*ROUTE PRINT NJOVC                                                             
//WDMR    EXEC WDMRP035                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ035                                         
