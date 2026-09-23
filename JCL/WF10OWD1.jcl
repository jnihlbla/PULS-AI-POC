//WF10OWD1 JOB (650WF100100WF10OWD1,W100),'RTN WF10D1',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DUMP     EXEC WG02DUMP,                                                       
//             UID=WF10OWD1,                                                    
//             DSOUT=WF01.DUMP.S01ONUW(+1)                                      
COPY TABLESPACE DWF01.S01ONUW SHRLEVEL CHANGE                                   
RUNSTATS TABLESPACE DWF01.S01ONUW INDEX(ALL) SHRLEVEL CHANGE                    
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WF10OWD1                                         
//*                                                                             
