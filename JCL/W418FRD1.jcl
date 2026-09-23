//W418FRD1 JOB (640W4180100W418FRD1,W100),'RTN W012V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//DUMP     EXEC WG02DUMP,COND.ABEND=(4,GE,DUMP),                                
//             UID=W418FRD1,                                                    
//             DSOUT=WG02.DUMP.SP8FRET(+1)                                      
COPY TABLESPACE DW418.SP8FRET SHRLEVEL CHANGE                                   
RUNSTATS TABLESPACE DW418.SP8FRET INDEX(ALL) SHRLEVEL CHANGE                    
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W418FRD1                                         
