//WDE7RLV9 JOB (640W0020200WDE7RLV9,W100),'RTN W010V9',                         
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
  DELETE (WG01.QASE.WDE7V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDE7V  DD DSN=WG01.QASE.WDE7V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(72000,3600),,CONTIG,ROUND),                           
//           MGMTCLAS=TP0,DATACLAS=MVOL,STORCLAS=TP0A                           
//*                                                                             
//WDE7    EXEC WG01REL,                                                         
//             DBD=WDE7                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(4096,(72000,3600),RLSE)                                   
//REL.DBORELD1 DD DSN=WG01.UNLO.WDE7V(+0),DISP=SHR                              
//REL.WDE7V DD DSN=WG01.QASE.WDE7V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDE7V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDE7ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE7BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDE7CCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDE7AK DD DSN=WG01.QASE.WDE7AK,DISP=SHR                                   
//SIU.WDE7BK DD DSN=WG01.QASE.WDE7BK,DISP=SHR                                   
//SIU.WDE7CK DD DSN=WG01.QASE.WDE7CK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDE7,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDE7RLV9                                         
