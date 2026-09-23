//WDR5RLV9 JOB (640W0020200WDR5RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDR5V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDR5V  DD DSN=WG01.QASE.WDR5V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(8192,(1080,180),,CONTIG,ROUND),                             
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDR5    EXEC WG01REL,                                                         
//             DBD=WDR5                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(8192,(1080,900),RLSE)                                     
//REL.DBORELD1 DD DSN=WG01.UNLO.WDR5V(+0),DISP=SHR                              
//REL.WDR5V DD DSN=WG01.QASE.WDR5V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR5RLV9                                         
