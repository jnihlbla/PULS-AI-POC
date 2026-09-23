//WDQ3SRV9 JOB (650W0020200WDQ3SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDQ3CLU                                          
//*                                                                             
//WDQ3    EXEC WG01REL,                                                         
//             DBD=WDQ3                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(27000,1800),RLSE),                                  
//             DCB=BUFNO=10,DATACLAS=MVOL                                       
//REL.DBORELD1 DD DSN=WG01.UNLO.WDQ3K(+0),DISP=SHR                              
//REL.WDQ3K DD DSN=WG01.QASE.WDQ3K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDQ3K,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDQ3ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3FCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3GCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3HCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3ICLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3JCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDQ3KCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDQ3AK DD DSN=WG01.QASE.WDQ3AK,DISP=SHR                                   
//SIU.WDQ3BK DD DSN=WG01.QASE.WDQ3BK,DISP=SHR                                   
//SIU.WDQ3CK DD DSN=WG01.QASE.WDQ3CK,DISP=SHR                                   
//SIU.WDQ3DK DD DSN=WG01.QASE.WDQ3DK,DISP=SHR                                   
//SIU.WDQ3EK DD DSN=WG01.QASE.WDQ3EK,DISP=SHR                                   
//SIU.WDQ3FK DD DSN=WG01.QASE.WDQ3FK,DISP=SHR                                   
//SIU.WDQ3GK DD DSN=WG01.QASE.WDQ3GK,DISP=SHR                                   
//SIU.WDQ3HK DD DSN=WG01.QASE.WDQ3HK,DISP=SHR                                   
//SIU.WDQ3IK DD DSN=WG01.QASE.WDQ3IK,DISP=SHR                                   
//SIU.WDQ3JK DD DSN=WG01.QASE.WDQ3JK,DISP=SHR                                   
//SIU.WDQ3KK DD DSN=WG01.QASE.WDQ3KK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:48                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDQ3,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDQ3SRV9                                         
