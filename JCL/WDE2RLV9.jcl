//WDE2RLV9 JOB (640W0020200WDE2RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDE2V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDE2V  DD DSN=WG01.QASE.WDE2V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(8192,(36000,6300),,CONTIG,ROUND),                           
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDE2    EXEC WG01REL,                                                         
//             DBD=WDE2                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(8192,(36000,6300),RLSE)                                   
//REL.DBORELD1 DD DSN=WG01.UNLO.WDE2V(+0),DISP=SHR                              
//REL.WDE2V DD DSN=WG01.QASE.WDE2V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDE2V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDE2ACLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDE2AK DD DSN=WG01.QASE.WDE2AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDE2,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDE2RLV9                                         
