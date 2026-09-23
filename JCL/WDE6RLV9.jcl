//WDE6RLV9 JOB (640W0020200WDE6RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDE6V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDE6V  DD DSN=WG01.QASE.WDE6V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(216000,6300),,CONTIG,ROUND),                          
//           MGMTCLAS=TP0,DATACLAS=MVOL,STORCLAS=TP0A                           
//*                                                                             
//DD1    DD DSN=WG01.QASE.WDE4V,DISP=SHR                                        
//DD2    DD DSN=WG01.QASE.WDE7V,DISP=SHR                                        
//*                                                                             
//WDE6    EXEC WG01REL,                                                         
//             DBD=WDE6                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(4096,(180000,6300),RLSE)                                  
//REL.DBORELD1 DD DSN=WG01.UNLO.WDE6V(+0),DISP=SHR                              
//REL.WDE6V DD DSN=WG01.QASE.WDE6V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDE6V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDE6ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE6BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE6CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE6DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE6ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE6FCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE6GCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE6HCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDE6AK DD DSN=WG01.QASE.WDE6AK,DISP=SHR                                   
//SIU.WDE6BK DD DSN=WG01.QASE.WDE6BK,DISP=SHR                                   
//SIU.WDE6CK DD DSN=WG01.QASE.WDE6CK,DISP=SHR                                   
//SIU.WDE6DK DD DSN=WG01.QASE.WDE6DK,DISP=SHR                                   
//SIU.WDE6EK DD DSN=WG01.QASE.WDE6EK,DISP=SHR                                   
//SIU.WDE6FK DD DSN=WG01.QASE.WDE6FK,DISP=SHR                                   
//SIU.WDE6GK DD DSN=WG01.QASE.WDE6GK,DISP=SHR                                   
//SIU.WDE6HK DD DSN=WG01.QASE.WDE6HK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDE6,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDE6RLV9                                         
