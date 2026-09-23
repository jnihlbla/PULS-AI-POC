//WDQ1SRV9 JOB (650W0020200WDQ1SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDQ1CLU                                          
//*                                                                             
//WDQ1    EXEC WG01REL,                                                         
//             DBD=WDQ1                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(126000,9000),RLSE),                                 
//             DCB=BUFNO=10,DATACLAS=MVOL                                       
//REL.DBORELD1 DD DSN=WG01.UNLO.WDQ1K(+0),DISP=SHR                              
//REL.WDQ1K DD DSN=WG01.QASE.WDQ1K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDQ1K,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDQ1ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ1BCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDQ1AK DD DSN=WG01.QASE.WDQ1AK,DISP=SHR                                   
//SIU.WDQ1BK DD DSN=WG01.QASE.WDQ1BK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:48                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDQ1,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDQ1SRV9                                         
