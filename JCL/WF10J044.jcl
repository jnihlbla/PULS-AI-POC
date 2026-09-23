//WF10J044 JOB (640WF100100WF10J044,W100),'RTN WF10V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WF10    EXEC WF10P044                                                         
//TSO.SYSTSIN DD *                                                              
DSN SYS(D2G0)                                                                   
RUN PROG(WF1044) PLAN (WF1044) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J044                                         
