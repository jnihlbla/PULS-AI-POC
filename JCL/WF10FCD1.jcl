//WF10FCD1 JOB (650WF100100WF10FCD1,W100),'RTN WF10D1',                         
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
//             UID=WF10FCD1,                                                    
//             DSOUT=WF01.DUMP.S01FCUS(+1)                                      
COPY TABLESPACE DWF01.S01FCUS SHRLEVEL CHANGE                                   
RUNSTATS TABLESPACE DWF01.S01FCUS INDEX(ALL) SHRLEVEL CHANGE                    
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WF10FCD1                                         
//*                                                                             
