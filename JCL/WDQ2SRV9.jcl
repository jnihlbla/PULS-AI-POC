//WDQ2SRV9 JOB (650W0020200WDQ2SRV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDQ2V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDQ2V  DD DSN=WG01.QASE.WDQ2V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(450000,18000),,CONTIG,ROUND),                         
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDQ2    EXEC WG01REL,                                                         
//             DBD=WDQ2                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(450000,18000),RLSE),                                
//             DCB=BUFNO=10,DATACLAS=MVOL                                       
//REL.DBORELD1 DD DSN=WG01.UNLO.WDQ2V(+0),DISP=SHR                              
//REL.WDQ2V DD DSN=WG01.QASE.WDQ2V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDQ2V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDQ2BCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDQ2CCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDQ2DCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDQ2ECLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDQ2FCLU),DISP=SHR                           
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDQ2BK DD DSN=WG01.QASE.WDQ2BK,DISP=SHR                                   
//SIU.WDQ2CK DD DSN=WG01.QASE.WDQ2CK,DISP=SHR                                   
//SIU.WDQ2DK DD DSN=WG01.QASE.WDQ2DK,DISP=SHR                                   
//SIU.WDQ2EK DD DSN=WG01.QASE.WDQ2EK,DISP=SHR                                   
//SIU.WDQ2FK DD DSN=WG01.QASE.WDQ2FK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:48                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDQ2,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDQ2SRV9                                         
