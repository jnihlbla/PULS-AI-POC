//WF10CWD1 JOB (650WF100100WF10CWD1,W100),'RTN WF10D1',                         
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
//             UID=WF10CWD1,                                                    
//             DSOUT=WF01.DUMP.S01CCTW(+1)                                      
COPY TABLESPACE DWF01.S01CCTW                                                   
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WF10CWD1                                         
//*                                                                             
