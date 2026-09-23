//W219J014 JOB (640W2190100W219J014,W100),'RTN W219S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W219    EXEC W219P014                                                         
//*                                                                             
//TSO.SYSTSIN DD *                                                              
DSN SYS(D2G0)                                                                   
RUN PROG(W21914) PLAN(W21914) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W219J014                                         
