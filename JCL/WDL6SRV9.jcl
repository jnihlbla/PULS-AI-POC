//WDL6SRV9 JOB (650W0020200WDL6SRV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDL6V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDL6V  DD DSN=WG01.QASE.WDL6V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(8192,(250000,27000),,CONTIG,ROUND),                         
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDL6    EXEC WG01REL,                                                         
//             DBD=WDL6                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(8192,(250000,27000),RLSE),                                
//             DCB=BUFNO=10                                                     
//REL.DBORELD1 DD DSN=WG01.UNLO.WDL6V(+0),DISP=SHR                              
//REL.WDL6V DD DSN=WG01.QASE.WDL6V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDL6V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDL6ACLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDL6BCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDL6CCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDL6DCLU),DISP=SHR                           
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDL6AK DD DSN=WG01.QASE.WDL6AK,DISP=SHR                                   
//SIU.WDL6BK DD DSN=WG01.QASE.WDL6BK,DISP=SHR                                   
//SIU.WDL6CK DD DSN=WG01.QASE.WDL6CK,DISP=SHR                                   
//SIU.WDL6DK DD DSN=WG01.QASE.WDL6DK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:45                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDL6,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDL6SRV9                                         
