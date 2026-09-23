//W371J09C JOB (650W3710100W371J09C,W100),'RTN W371Y1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//*+JBS BIND D2G0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W371P09C EXEC W371P09C                                                        
//W3719C.SYSTSIN  DD  *                                                         
DSN SYS(D2G0)                                                                   
RUN PROG(W3719C) PLAN(W3719C) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W371J09C                                            
