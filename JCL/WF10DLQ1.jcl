//WF10DLQ1 JOB (650WF100100WF10DLQ1,W100),'RTN WF10D5',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DISCARD EXEC PROC=WG02REOT,SYSTEM=D2G0,DB2T=DLIN,                             
//             INDRTE=WF01,JOBNAME=WF10DLQ1,UID=WF10DLQ1                        
//*                                                                             
//REOR.SYSPUNCH DD DSN=&INDRTE..SYSPUNCH.S01&DB2T.(+1),                         
//*            SYSPUNCH DATA - (LOAD CARD)                                      
//             DISP=(,CATLG,DELETE),                                            
//             SPACE=(TRK,(900,300),RLSE),                                      
//             MGMTCLAS=&BACKUPC,DATACLAS=MVOL                                  
//REOR.SYSDISC DD DSN=&INDRTE..DISCARD.S01&DB2T.(+1),                           
//*            DISCARDED RECORDS                                                
//             DISP=(,CATLG,DELETE),                                            
//             SPACE=(TRK,(900,300),RLSE),                                      
//             MGMTCLAS=&BACKUPC,DATACLAS=MVOL                                  
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10DLQ1                                         
//*                                                                             
