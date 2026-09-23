//WPVVAIXD JOB (650W0020200WPVVAIXD,W100),'RTN W012V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DUMP     EXEC WG02DUMP,DSOUT=WG02.DUMP.PVVAIX(+1),                            
//             UID=WPVVAIXD                                                     
COPY TABLESPACE DWBOPS.PVVAIX                                                   
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WPVVAIXD                                         
