//WZ14J001 JOB (640WZ140100WZ14J001,W100),'RTN WZ14D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*+JBS BIND D2G0                                                               
//*                                                                             
//** WAIT 5 MIN TO PREVENT CRASH WITH WZ0420-TRANS                              
//WAIT    EXEC WWAIT,SECONDS=300                                                
//*                                                                             
//WZ14     EXEC WZ14P001                                                        
//WZ1401.SYSTSIN  DD  *                                                         
DSN SYS(D2G0)                                                                   
RUN PROG(WZ1401) LIB('W.QASE.LOAD')                                             
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WZ14J001                                         
