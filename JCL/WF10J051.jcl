//WF10J051 JOB (640WF100100WF10J051,W100),'RTN WF10M2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WF10    EXEC WF10P051                                                         
//TSO.SYSTSIN DD *                                                              
DSN SYS(D2G0)                                                                   
RUN PROG(WF1051) PLAN (WF1051) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J051                                         
