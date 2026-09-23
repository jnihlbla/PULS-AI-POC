//W430PAL1 JOB (670W4300100W430PAL1,W100),'RTN W430D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W430.W430D1.W43061(+0),                           
//             TTLOAD=TP6PARLO,UID=W430PAL1,JOBNAME=W430PAL1                    
//SYSIN DD DSN=W.PROD.DDL(TP6PARLO)                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W430PAL1                                         
