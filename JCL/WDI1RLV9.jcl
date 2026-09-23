//WDI1RLV9 JOB (640W0020200WDI1RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDI1V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDI1V  DD DSN=WG01.QASE.WDI1V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(36000,1800),,CONTIG,ROUND),                           
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDI1    EXEC WG01REL,                                                         
//             DBD=WDI1                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(36000,1800),RLSE)                                   
//REL.DBORELD1 DD DSN=WG01.UNLO.WDI1V(+0),DISP=SHR                              
//REL.WDI1V DD DSN=WG01.QASE.WDI1V,DISP=SHR                                     
//*                                                                             
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDI1V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDI1ACLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDI1AK DD DSN=WG01.QASE.WDI1AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:44                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDI1,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDI1RLV9                                         
