//WDMRJ074 JOB (640W0030200WDMRJ074,W100),'RTN WDMRV4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJEVC                                                             
/*ROUTE PRINT LOCAL                                                             
//WDMR    EXEC WDMRP074                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ074                                         
