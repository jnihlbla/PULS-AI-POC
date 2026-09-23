//WF10J001 JOB (640WF100100WF10J001,W100),'RTN WF10D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WF10    EXEC WF10P001                                                         
//WF1001.SYSTSIN  DD  *                                                         
DSN SYS(D2G0)                                                                   
RUN PROG(WF1001) PLAN(WF1001) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10J001                                         
