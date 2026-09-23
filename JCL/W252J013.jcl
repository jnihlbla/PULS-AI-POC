//W252J013 JOB (510W2520100W252J013,W100),'RTN W252D1',                         
//             CLASS=K,USER=?,PASSWORD=?,                                       
//             MSGLEVEL=(1,1)                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W252    EXEC W252P013                                                         
//TSO.SYSTSIN  DD  *                                                            
DSN SYS(D2G0)                                                                   
RUN PROG(W25213) PLAN (W25213) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W252J013                                         
