//W015WLD1 JOB (640W4830100WL15WLD1,W100),'RTN W015D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE    XEQ LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*+JBS BIND D2G0                                                               
//*                                                                             
//DUMP     EXEC WG02DUMP,                                                       
//             UID=W015WLD1,                                                    
//             DSOUT=W015.DUMP.SP0WLOG(+1)                                      
//SYSCOPY DD SPACE=,DATACLAS=PSEB                                               
COPY     TABLESPACE DWZ01.STP0WLOG                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W015WLD1                                         
