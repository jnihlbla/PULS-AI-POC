//WDA6SIV9 JOB (640W0020200WDA6SIV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//SYSIN    DD DSN=W.QASE.CONSTANT(WDA6ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6FCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6GCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6HCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6ICLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6JCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6KCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6LCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDA6MCLU),DISP=SHR                            
//*                                                                             
//WDA6    EXEC WG01SIU                                                          
//SIU.WDA6V  DD DSN=WG01.QASE.WDA6V,DISP=SHR                                    
//SIU.WDA6AK DD DSN=WG01.QASE.WDA6AK,DISP=SHR                                   
//SIU.WDA6BK DD DSN=WG01.QASE.WDA6BK,DISP=SHR                                   
//SIU.WDA6CK DD DSN=WG01.QASE.WDA6CK,DISP=SHR                                   
//SIU.WDA6DK DD DSN=WG01.QASE.WDA6DK,DISP=SHR                                   
//SIU.WDA6EK DD DSN=WG01.QASE.WDA6EK,DISP=SHR                                   
//SIU.WDA6FK DD DSN=WG01.QASE.WDA6FK,DISP=SHR                                   
//SIU.WDA6GK DD DSN=WG01.QASE.WDA6GK,DISP=SHR                                   
//SIU.WDA6HK DD DSN=WG01.QASE.WDA6HK,DISP=SHR                                   
//SIU.WDA6IK DD DSN=WG01.QASE.WDA6IK,DISP=SHR                                   
//SIU.WDA6JK DD DSN=WG01.QASE.WDA6JK,DISP=SHR                                   
//SIU.WDA6KK DD DSN=WG01.QASE.WDA6KK,DISP=SHR                                   
//SIU.WDA6LK DD DSN=WG01.QASE.WDA6LK,DISP=SHR                                   
//SIU.WDA6MK DD DSN=WG01.QASE.WDA6MK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDA6,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDA6SIV9                                         
