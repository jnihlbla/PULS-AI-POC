//WDA3RLV9 JOB (640W0020200WDA3RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDA3V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDA3V  DD DSN=WG01.QASE.WDA3V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(720,900),,CONTIG,ROUND),                              
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDA3    EXEC WG01REL,                                                         
//             DBD=WDA3                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(4096,(720,900),RLSE)                                      
//REL.DBORELD1 DD DSN=WG01.UNLO.WDA3V(+0),DISP=SHR                              
//REL.WDA3V DD DSN=WG01.QASE.WDA3V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDA3V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDA3ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA3BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA3CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA3DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA3ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA3FCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA3GCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA3HCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA3ICLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDA3AK DD DSN=WG01.QASE.WDA3AK,DISP=SHR                                   
//SIU.WDA3BK DD DSN=WG01.QASE.WDA3BK,DISP=SHR                                   
//SIU.WDA3CK DD DSN=WG01.QASE.WDA3CK,DISP=SHR                                   
//SIU.WDA3DK DD DSN=WG01.QASE.WDA3DK,DISP=SHR                                   
//SIU.WDA3EK DD DSN=WG01.QASE.WDA3EK,DISP=SHR                                   
//SIU.WDA3FK DD DSN=WG01.QASE.WDA3FK,DISP=SHR                                   
//SIU.WDA3GK DD DSN=WG01.QASE.WDA3GK,DISP=SHR                                   
//SIU.WDA3HK DD DSN=WG01.QASE.WDA3HK,DISP=SHR                                   
//SIU.WDA3IK DD DSN=WG01.QASE.WDA3IK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDA3,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDA3RLV9                                         
