//WF10SDQ1 JOB (650WF100100WF10SDQ1,W100),'RTN WF10M1',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DISCARD EXEC PROC=WG02REOF,SYSTEM=D2G0,DB2T=SDEV,                             
//             JOBNAME=WF10SDQ1,UID=WF10SDQ1,INDRTE=WF01                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10SDQ1                                         
//*                                                                             
