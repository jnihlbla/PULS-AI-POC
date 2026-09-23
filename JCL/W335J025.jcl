//W335J025 JOB (640W3350100W335J025,W100),'RTN W335V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W335    EXEC W335P025                                                         
//*                                                                             
//TSO.SYSTSIN DD *                                                              
DSN SYS(D2G0)                                                                   
RUN PROG(W33525) PLAN (W33525) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J025                                         
