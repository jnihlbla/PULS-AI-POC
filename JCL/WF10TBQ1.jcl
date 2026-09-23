//WF10TBQ1 JOB (650WF100100WF10TBQ1,W100),'RTN WF10D5',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DISCARD EXEC PROC=WG02REOT,SYSTEM=D2G0,DB2T=TBUN,                             
//             INDRTE=WF01,JOBNAME=WF10TBQ1,UID=WF10TBQ1                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10TBQ1                                         
//*                                                                             
