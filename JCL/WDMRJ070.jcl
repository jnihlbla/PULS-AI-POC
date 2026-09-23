//WDMRJ070 JOB (640W0030200WDMRJ070,W100),'RTN WDMRV4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJEVC                                                             
/*ROUTE PRINT LOCAL                                                             
//WDMR    EXEC WDMRP070                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ070                                         
