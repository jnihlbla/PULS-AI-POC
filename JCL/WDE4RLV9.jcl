//WDE4RLV9 JOB (640W0020200WDE4RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDE4V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDE4V  DD DSN=WG01.QASE.WDE4V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(8192,(216000,6300),,CONTIG,ROUND),                          
//           MGMTCLAS=TP0,DATACLAS=MVOL,STORCLAS=TP0A                           
//*                                                                             
//DD1    DD DSN=WG01.QASE.WDE6V,DISP=SHR                                        
//DD2    DD DSN=WG01.QASE.WDE7V,DISP=SHR                                        
//*                                                                             
//WDE4    EXEC WG01REL,                                                         
//             DBD=WDE4                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(8192,(106500,6300),RLSE)                                  
//REL.DBORELD1 DD DSN=WG01.UNLO.WDE4V(+0),DISP=SHR                              
//REL.WDE4V DD DSN=WG01.QASE.WDE4V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDE4V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDE4ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE4BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE4CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE4DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE4ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE4FCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDE4AK DD DSN=WG01.QASE.WDE4AK,DISP=SHR                                   
//SIU.WDE4BK DD DSN=WG01.QASE.WDE4BK,DISP=SHR                                   
//SIU.WDE4CK DD DSN=WG01.QASE.WDE4CK,DISP=SHR                                   
//SIU.WDE4DK DD DSN=WG01.QASE.WDE4DK,DISP=SHR                                   
//SIU.WDE4EK DD DSN=WG01.QASE.WDE4EK,DISP=SHR                                   
//SIU.WDE4FK DD DSN=WG01.QASE.WDE4FK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDE4,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDE4RLV9                                         
