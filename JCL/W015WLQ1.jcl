//W015WLQ1 JOB (670W0090100W015WLQ1,W100),'RTN W015M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DISCARD EXEC PROC=WG02REO2,SYSTEM=D2G0,DB2T=WLOG,S01=SP0,                     
//             JOBNAME=W015WLQ1,UID=W015WLQ1,INDRTE=W015,DAT2=DAT0              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W015WLQ1                                         
//*                                                                             
