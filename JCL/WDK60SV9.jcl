//WDK60SV9 JOB (640W0020200WDK60SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//SYSIN    DD DSN=W.QASE.CONSTANT(WDK6ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDK6BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDK6CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDK6DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDK6ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDK6FCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDK6GCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDK6HCLU),DISP=SHR                            
//*                                                                             
//WDK6    EXEC WG01SIU                                                          
//SIU.WDK6V  DD DSN=WG01.QASE.WDK6V,DISP=SHR                                    
//SIU.WDK6AK DD DSN=WG01.QASE.WDK6AK,DISP=SHR                                   
//SIU.WDK6BK DD DSN=WG01.QASE.WDK6BK,DISP=SHR                                   
//SIU.WDK6CK DD DSN=WG01.QASE.WDK6CK,DISP=SHR                                   
//SIU.WDK6DK DD DSN=WG01.QASE.WDK6DK,DISP=SHR                                   
//SIU.WDK6EK DD DSN=WG01.QASE.WDK6EK,DISP=SHR                                   
//SIU.WDK6FK DD DSN=WG01.QASE.WDK6FK,DISP=SHR                                   
//SIU.WDK6GK DD DSN=WG01.QASE.WDK6GK,DISP=SHR                                   
//SIU.WDK6HK DD DSN=WG01.QASE.WDK6HK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:45                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDK6,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDK60SV9                                         
