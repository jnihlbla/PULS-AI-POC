//WDI2RLV9 JOB (640W0020200WDI2RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDI2V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDI2V  DD DSN=WG01.QASE.WDI2V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(27000,1800),,CONTIG,ROUND),                           
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDI2    EXEC WG01REL,                                                         
//             DBD=WDI2                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDI2V(+0),DISP=SHR                              
//REL.WDI2V DD DSN=WG01.QASE.WDI2V,DISP=SHR                                     
//*                                                                             
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDI2RLV9                                         
