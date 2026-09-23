//WDR4SRV9 JOB (650W0020200WDR4SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS EXEC PGM=IDCAMS                                                        
//SYSPRINT DD SYSOUT=*                                                          
  DELETE (WG01.QASE.WDR4V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDR4V  DD DSN=WG01.QASE.WDR4V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(8192,(3600,360),,CONTIG,ROUND),                             
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDR4    EXEC WG01REL,                                                         
//             DBD=WDR4,COND.ABEND=(4,GE,REL)                                   
//REL.DBORELD1 DD DSN=WG01.UNLO.WDR4V(+0),DISP=SHR                              
//REL.WDR4V DD DSN=WG01.QASE.WDR4V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR4SRV9                                         
