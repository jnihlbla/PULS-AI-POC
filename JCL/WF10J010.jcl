//WF10J010 JOB (640WF100100WF10J010,W100),'RTN WF10D1',                         
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
//WF10P010 EXEC WF10P010                                                        
//TSO.SYSTSIN  DD  *                                                            
DSN SYS(D2G0)                                                                   
RUN PROG(WF1010) PLAN(WF1010) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//END  EXEC WSOPEND,PROCESS=WF10J010                                            
//*                                                                             
