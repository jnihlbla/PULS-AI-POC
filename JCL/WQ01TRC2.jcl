//WQ01TRC2 JOB (640W4830100WQ01TRC2,W100),'RTN W483V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//SQLBTCH EXEC PGM=IKJEFT01,DYNAMNBR=20                                         
//SYSTSPRT DD  SYSOUT=*                                                         
//SYSPRINT DD  SYSOUT=*                                                         
//SYSUDUMP DD  SYSOUT=*                                                         
//SYSTSIN  DD  *                                                                
    DSN SYSTEM(D2G0)                                                            
       RUN PROGRAM(DSNTEP2)  PLAN(DSNTEP2)                                      
    END                                                                         
//* DELETE LINES WITH LOADING DATE OLDER THAN TODAY - 3 YEARS                   
//SYSIN    DD  *                                                                
 DELETE                                                                         
 FROM  WDB2.TQ1TRPUP                                                            
 WHERE TILASTN BETWEEN 0 AND (&SYSDATE - 30000)                                 
 ;                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WQ01TRC2                                         
