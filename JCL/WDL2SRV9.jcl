//WDL2SRV9 JOB (650W0020200WDL2SRV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDL2V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDL2V  DD DSN=WG01.QASE.WDL2V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(2048,(252000,12600),,CONTIG,ROUND),                         
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDL2    EXEC WG01REL,                                                         
//             DBD=WDL2                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDL2V(+0),DISP=SHR                              
//REL.WDL2V DD DSN=WG01.QASE.WDL2V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL2SRV9                                         
