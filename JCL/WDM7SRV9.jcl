//WDM7SRV9 JOB (640W0020200WDM7SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*                                                                              
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDM7CLU                                          
//*                                                                             
//WDM7    EXEC WG01REL,                                                         
//             DBD=WDM7                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(2800,900),RLSE),                                    
//             DCB=BUFNO=10                                                     
//REL.DBORELD1 DD DSN=WG01.UNLO.WDM7K(+0),DISP=SHR                              
//REL.WDM7K DD DSN=WG01.QASE.WDM7K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDM7K,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDM7ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDM7BCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDM7AK DD DSN=WG01.QASE.WDM7AK,DISP=SHR                                   
//SIU.WDM7BK DD DSN=WG01.QASE.WDM7BK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDM7,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDM7SRV9                                         
//*                                                                             
