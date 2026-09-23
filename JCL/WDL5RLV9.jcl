//WDL5RLV9 JOB (640W0020200WDL5RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDL5V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDL5V  DD DSN=WG01.QASE.WDL5V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(16384,(180000,18000),,CONTIG,ROUND),                        
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDL5    EXEC WG01REL,                                                         
//             DBD=WDL5                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(16384,(180000,18000),RLSE)                                
//REL.DBORELD1 DD DSN=WG01.UNLO.WDL5V(+0),DISP=SHR                              
//REL.WDL5V DD DSN=WG01.QASE.WDL5V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDL5V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDL5ACLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDL5AK DD DSN=WG01.QASE.WDL5AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDL5,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDL5RLV9                                         
