//W430PAD1 JOB (670W4300100W430PAD1,W100),'RTN W430D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DUMP     EXEC WG02DUMP,                                                       
//             UID=W430PAD1,COND.ABEND=(4,GE,DUMP),                             
//             DSOUT=W430.DUMP.SP6PART(+1)                                      
COPY TABLESPACE DWL10.SP6PART SHRLEVEL CHANGE                                   
MODIFY RECOVERY TABLESPACE DWL10.SP6PART DELETE AGE(70)                         
RUNSTATS TABLESPACE DWL10.SP6PART INDEX(ALL) SHRLEVEL CHANGE                    
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W430PAD1                                         
//*                                                                             
