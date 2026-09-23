//WPVBUDGD JOB (650W0020200WPVBUDGD,W100),'RTN W012V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DUMP     EXEC WG02DUMP,DSOUT=WG02.DUMP.PVBUDG(+1),                            
//             UID=WPVBUDGD                                                     
COPY TABLESPACE DWBOPS.PVBUDG                                                   
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WPVBUDGD                                         
