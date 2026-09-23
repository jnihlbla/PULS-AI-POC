//WDQ4SRV9 JOB (650W0020200WDQ4SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDQ4CLU                                          
//*                                                                             
//WDQ4    EXEC WG01REL,                                                         
//             DBD=WDQ4                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(54000,3600),RLSE),                                  
//             DCB=BUFNO=10,DATACLAS=MVOL                                       
//REL.DBORELD1 DD DSN=WG01.UNLO.WDQ4K(+0),DISP=SHR                              
//REL.WDQ4K DD DSN=WG01.QASE.WDQ4K,DISP=SHR                                     
//*                                                                             
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDQ4K,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDQ4ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ4BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ4CCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDQ4AK DD DSN=WG01.QASE.WDQ4AK,DISP=SHR                                   
//SIU.WDQ4BK DD DSN=WG01.QASE.WDQ4BK,DISP=SHR                                   
//SIU.WDQ4CK DD DSN=WG01.QASE.WDQ4CK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:48                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDQ4,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDQ4SRV9                                         
