//WF20J032 JOB (640WF200100WF20J032,W100),'RTN WF20S4',                         
//             USER=?,PASSWORD=?,                                               
//             MSGLEVEL=(1,1),                                                  
//             CLASS=K                                                          
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//*+JBS BIND D2G0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*CNTL  WSOPDAT,EXC                                                             
//*                                                                             
//WF20P032 EXEC WF20P032                                                        
//TSO.SYSTSIN  DD  *                                                            
DSN SYS(D2G0)                                                                   
RUN PROG(WF2032) PLAN(WF2032) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//ORDER  EXEC WSOP,COMMAND='ACTIVATE WF21S1'                                    
//ORDER  EXEC WSOP,COMMAND='ACTIVATE WF23S1'                                    
//*                                                                             
//END  EXEC WSOPEND,PROCESS=WF20J032                                            
