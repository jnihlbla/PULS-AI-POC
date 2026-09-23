//W522J071 JOB (640W5220100W522J071,W100),'RTN W522D1',                         
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
//W522    EXEC W522P071                                                         
//TSO.SYSTSIN  DD *                                                             
DSN SYS(D2G0)                                                                   
RUN PROG(W52271) PLAN(W52271) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W522J071                                         
