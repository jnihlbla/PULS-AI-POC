//WDMRJ077 JOB (640W0030200WDMRJ077,W100),'RTN WDMRV4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJESD                                                             
/*ROUTE PRINT NJOVC                                                             
//WDMR    EXEC WDMRP077                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ077                                         
