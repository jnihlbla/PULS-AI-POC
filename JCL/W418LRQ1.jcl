//W418LRQ1 JOB (670W4180100W418LRQ1,W100),'RTN W418M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DISCARD EXEC PROC=WG02REO2,SYSTEM=D2G0,DB2T=LRET,S01=SP8,                     
//             JOBNAME=W418LRQ1,UID=W418LRQ1,INDRTE=W418,DAT2=DAT7              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W418LRQ1                                         
//*                                                                             
