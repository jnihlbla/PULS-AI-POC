//W512J093 JOB (640W5120100W512J093,W100),'RTN W512M2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W512    EXEC W512P093                                                         
//TSO.SYSTSIN DD *                                                              
DSN SYS(D2G0)                                                                   
RUN PROG(W51293) PLAN (W51293) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W512J093                                         
