//WF10J081 JOB (640WF100100WF10J081,W100),'RTN WF10M1',                         
//             USER=?,PASSWORD=?,                                               
//             MSGLEVEL=(1,1),                                                  
//             CLASS=K                                                          
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//*+JBS BIND D2G0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WF1081   EXEC WF10P081                                                        
//*                                                                             
//TSO.WF1081D1 DD DSN=WF01.REOR.DAEXDAT1(+1),DISP=(,CATLG),                     
//             SPACE=(TRK,(1,1))                                                
//*                                                                             
//TSO.SYSTSIN  DD  *                                                            
DSN SYS(D2G0)                                                                   
RUN PROG(WF1081) PLAN(WF1081) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10J081                                         
