//WDJ7SRV9 JOB (650W0020200WDJ7SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDJ7CLU                                          
//*                                                                             
//WDJ7    EXEC WG01REL,                                                         
//             DBD=WDJ7                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(4096,(18000,1800),RLSE)                                   
//REL.DBORELD1 DD DSN=WG01.UNLO.WDJ7K(+0),DISP=SHR                              
//REL.WDJ7K DD DSN=WG01.QASE.WDJ7K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDJ7K,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDJ7ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDJ7BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDJ7CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDJ7DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDJ7ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDJ7FCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDJ7AK DD DSN=WG01.QASE.WDJ7AK,DISP=SHR                                   
//SIU.WDJ7BK DD DSN=WG01.QASE.WDJ7BK,DISP=SHR                                   
//SIU.WDJ7CK DD DSN=WG01.QASE.WDJ7CK,DISP=SHR                                   
//SIU.WDJ7DK DD DSN=WG01.QASE.WDJ7DK,DISP=SHR                                   
//SIU.WDJ7EK DD DSN=WG01.QASE.WDJ7EK,DISP=SHR                                   
//SIU.WDJ7FK DD DSN=WG01.QASE.WDJ7FK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:44                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDJ7,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDJ7SRV9                                         
