//W430ARL1 JOB (670W4300100W430ARL1,W100),'RTN W430V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W430.W430V1.W43060(+0),                           
//             TTLOAD=TP6ARTLO,UID=W430ARL1,JOBNAME=W430ARL1                    
//SYSIN DD DSN=W.PROD.DDL(TP6ARTLO)                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W430ARL1                                         
