//WF10ALD2 JOB (650WF100100WF10ALD2,W100),'RTN WF10D6',                         
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
//             UID=WF10ALD2,                                                    
//             DSOUT=WF01.DUMP.S01ALIN(+1)                                      
COPY TABLESPACE DWF01.S01ALIN SHRLEVEL CHANGE                                   
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WF10ALD2                                         
//*                                                                             
