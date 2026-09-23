//W335PAD1 JOB (650W3350100W335PAD1,W100),'RTN W335S1',                         
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
//             UID=W335PAD1,                                                    
//             DSOUT=W335.DUMP.SP7PART(+1)                                      
COPY TABLESPACE DW335.SP7PART                                                   
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W335PAD1                                         
//*                                                                             
