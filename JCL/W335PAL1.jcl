//W335PAL1 JOB (640W3350100W335PAL1,W100),'RTN W335S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W335.W335S1.W33524(0),                            
//             TTLOAD=TP7PARLO,UID=W335PAL1,JOBNAME=W335PAL1                    
//SYSIN DD DSN=W.PROD.DDL(TP7PARLO)                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W335PAL1                                         
