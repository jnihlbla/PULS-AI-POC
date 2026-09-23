//WF20J011 JOB (640WF200100WF20J011,W100),'RTN WF20S2',                         
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
//WF20P011 EXEC WF20P011                                                        
//TSO.SYSTSIN  DD  *                                                            
DSN SYS(D2G0)                                                                   
RUN PROG(WF2011) PLAN(WF2011) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//END  EXEC WSOPEND,PROCESS=WF20J011                                            
//*                                                                             
