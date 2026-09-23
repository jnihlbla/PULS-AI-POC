//W522J061 JOB (640W5220100W522J061,W100),'RTN W522D1',                         
//             CLASS=K,USER=?,PASSWORD=?,                                       
//             MSGLEVEL=(1,1)                                                   
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W522    EXEC W522P061                                                         
//TSO.SYSTSIN  DD *                                                             
DSN SYS(D2G0)                                                                   
RUN PROG(W52261) PLAN(W52261) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W522J061                                         
