//WDM60SV9 JOB (640W0020200WDM60SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDM6V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDM6ACLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDM6BCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDM6CCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDM6DCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDM6ECLU),DISP=SHR                           
//*                                                                             
//WDM6    EXEC WG01SIU                                                          
//SIU.WDM6V  DD DSN=WG01.QASE.WDM6V,DISP=SHR                                    
//SIU.WDM6AK DD DSN=WG01.QASE.WDM6AK,DISP=SHR                                   
//SIU.WDM6BK DD DSN=WG01.QASE.WDM6BK,DISP=SHR                                   
//SIU.WDM6CK DD DSN=WG01.QASE.WDM6CK,DISP=SHR                                   
//SIU.WDM6DK DD DSN=WG01.QASE.WDM6DK,DISP=SHR                                   
//SIU.WDM6EK DD DSN=WG01.QASE.WDM6EK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDM6,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDM60SV9                                         
