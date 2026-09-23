//WDA5RLV9 JOB (650W0020200WDA5RLV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDA5CLU                                          
//*                                                                             
//WDA5    EXEC WG01REL,                                                         
//             DBD=WDA5                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(4096,(18000,1800),RLSE)                                   
//REL.DBORELD1 DD DSN=WG01.UNLO.WDA5K(+0),DISP=SHR                              
//REL.WDA5K DD DSN=WG01.QASE.WDA5K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDA5K,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDA5ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA5BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA5CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA5DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA5ECLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDA5AK DD DSN=WG01.QASE.WDA5AK,DISP=SHR                                   
//SIU.WDA5BK DD DSN=WG01.QASE.WDA5BK,DISP=SHR                                   
//SIU.WDA5CK DD DSN=WG01.QASE.WDA5CK,DISP=SHR                                   
//SIU.WDA5DK DD DSN=WG01.QASE.WDA5DK,DISP=SHR                                   
//SIU.WDA5EK DD DSN=WG01.QASE.WDA5EK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDA5,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDA5RLV9                                         
