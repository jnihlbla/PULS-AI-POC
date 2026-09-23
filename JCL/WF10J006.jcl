//WF10J006 JOB (640WF100100WF10J006,W100),'RTN WF10D1',                         
//             USER=?,PASSWORD=?,                                               
//             MSGLEVEL=(1,1),                                                  
//             CLASS=K                                                          
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//*+JBS BIND D2G0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WF10P006 EXEC WF10P006                                                        
//TSO.SYSTSIN  DD  *                                                            
DSN SYS(D2G0)                                                                   
RUN PROG(WF1006) PLAN(WF1006) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//END  EXEC WSOPEND,PROCESS=WF10J006                                            
//*                                                                             
