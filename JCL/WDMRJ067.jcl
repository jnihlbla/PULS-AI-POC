//WDMRJ067 JOB (640W0030200WDMRJ067,W100),'RTN WDMRV3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJESD                                                             
/*ROUTE PRINT NJOVC                                                             
//WDMR    EXEC WDMRP067                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ067                                         
