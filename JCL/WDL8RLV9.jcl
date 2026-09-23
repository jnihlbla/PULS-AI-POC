//WDL8RLV9 JOB (640W0020200WDL8RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDL8V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDL8V  DD DSN=WG01.QASE.WDL8V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(16384,(162000,9000),,CONTIG,ROUND),                         
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDL8    EXEC WG01REL,                                                         
//             DBD=WDL8                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDL8V(+0),DISP=SHR                              
//REL.WDL8V DD DSN=WG01.QASE.WDL8V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL8RLV9                                         
