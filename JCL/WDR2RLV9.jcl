//WDR2RLV9 JOB (640W0020200WDR2RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDR2V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDR2V  DD DSN=WG01.QASE.WDR2V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(8192,(6300,90),,CONTIG,ROUND),                              
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDR2    EXEC WG01REL,                                                         
//             DBD=WDR2                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDR2V(+0),DISP=SHR                              
//REL.WDR2V DD DSN=WG01.QASE.WDR2V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR2RLV9                                         
