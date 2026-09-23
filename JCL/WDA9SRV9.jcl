//WDA9SRV9 JOB (650W0020200WDA9SRV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDA9V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDA9V  DD DSN=WG01.QASE.WDA9V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(5040,360),,CONTIG,ROUND),                             
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDA9    EXEC WG01REL,                                                         
//             DBD=WDA9,COND.ABEND=(4,GE,REL)                                   
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(5040,360),RLSE),                                    
//             DCB=BUFNO=10                                                     
//REL.DBORELD1 DD DSN=WG01.UNLO.WDA9V(+0),DISP=SHR                              
//REL.WDA9V DD DSN=WG01.QASE.WDA9V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDA9V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDA9ACLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDA9BCLU),DISP=SHR                           
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDA9AK DD DSN=WG01.QASE.WDA9AK,DISP=SHR                                   
//SIU.WDA9BK DD DSN=WG01.QASE.WDA9BK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDA9,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDA9SRV9                                         
