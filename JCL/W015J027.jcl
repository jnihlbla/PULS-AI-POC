//W015J027 JOB (670W5110100W015J026,W100),'RTN W015D2',                         
//             CLASS=K,USER=?,PASSWORD=?,                                       
//             MSGLEVEL=(1,1)                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND D2G0                                                               
//W015    EXEC W015P027                                                         
//*                                                                             
//TSO.SYSTSIN  DD  *                                                            
DSN SYS(D2G0)                                                                   
RUN PROG(W01527) PLAN(W01527)                                                   
END                                                                             
//*                                                                             
//TSO.W01527D3 DD DSN=W015.REOR.DAEXDAT0(+1)                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W015J027                                         
