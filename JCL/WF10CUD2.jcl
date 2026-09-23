//WF10CUD2 JOB (650WF100100WF10CUD2,W100),'RTN WF10R2',                         
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
//             UID=WF10CUD2,                                                    
//             DSOUT=WF01.DUMP.S01CURR(+1)                                      
COPY TABLESPACE DWF01.S01CURR                                                   
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WF10CUD2                                         
//*                                                                             
