//WDA2RLV9 JOB (640W0020200WDA2RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDA2V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDA2V  DD DSN=WG01.QASE.WDA2V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(8192,(220000,9000),,CONTIG,ROUND),                          
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDA2    EXEC WG01REL,                                                         
//             DBD=WDA2                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(8192,(180000,9000),RLSE)                                  
//REL.DBORELD1 DD DSN=WG01.UNLO.WDA2V(+0),DISP=SHR                              
//REL.WDA2V DD DSN=WG01.QASE.WDA2V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDA2V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDA2ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA2BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA2CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA2DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA2ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA2FCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA2GCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDA2AK DD DSN=WG01.QASE.WDA2AK,DISP=SHR                                   
//SIU.WDA2BK DD DSN=WG01.QASE.WDA2BK,DISP=SHR                                   
//SIU.WDA2CK DD DSN=WG01.QASE.WDA2CK,DISP=SHR                                   
//SIU.WDA2DK DD DSN=WG01.QASE.WDA2DK,DISP=SHR                                   
//SIU.WDA2EK DD DSN=WG01.QASE.WDA2EK,DISP=SHR                                   
//SIU.WDA2FK DD DSN=WG01.QASE.WDA2FK,DISP=SHR                                   
//SIU.WDA2GK DD DSN=WG01.QASE.WDA2GK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDA2,INDD=DFSURWF1,ICNEEDED=OFF                  
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDA2RLV9                                         
