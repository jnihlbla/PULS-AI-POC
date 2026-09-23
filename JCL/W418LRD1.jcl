//W418LRD1 JOB (670W4180100W418LRD1,W100),'RTN W418D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DUMP     EXEC WG02DUMP,                                                       
//             UID=W418LRD1,                                                    
//             DSOUT=W418.DUMP.SP8LRET(+1)                                      
COPY TABLESPACE DW418.SP8LRET                                                   
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W418LRD1                                         
//*                                                                             
