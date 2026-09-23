//W430ARQ1 JOB (670W4300100W430ARQ1,W100),'RTN W430V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DISCARD EXEC PROC=WG02REO2,SYSTEM=D2G0,DB2T=ARTP,S01=SP6,                     
//             JOBNAME=W430ARQ1,UID=W430ARQ1,INDRTE=W430,DAT2=DAT2              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W430ARQ1                                         
//*                                                                             
