//W418LRL1 JOB (670W4180100W418LRL1,W100),'RTN W418D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W418.W418D4.W418AX(+0),                           
//             TTLOAD=TP8LRELO,UID=W418LRL1,JOBNAME=W418LRL1                    
//SYSIN DD DSN=W.PROD.DDL(TP8LRELO)                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W418LRL1                                         
