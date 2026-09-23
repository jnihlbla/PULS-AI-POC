//WB11J020 JOB (640WB010100WB11J020,W100),'RTN WB01D1',                         
//             CLASS=K,MSGLEVEL=(1,1),                                          
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTB                                                    
/*JOBPARM FORMS=1800,LINECT=0,LINES=99                                          
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WB11    EXEC WB11P020                                                         
//TSO.SYSTSIN  DD  *                                                            
DSN SYS(D2G0)                                                                   
RUN PROG(WB1120) PLAN(WB1120) LIB('W.QASE.LOAD')                                
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WB11J020                                         
