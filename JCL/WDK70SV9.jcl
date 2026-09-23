//WDK70SV9 JOB (640W0020200WDK70SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//SYSIN    DD DSN=W.QASE.CONSTANT(WDK7ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDK7BCLU),DISP=SHR                            
//*                                                                             
//WDK7    EXEC WG01SIU                                                          
//SIU.WDK7V  DD DSN=WG01.QASE.WDK7V,DISP=SHR                                    
//SIU.WDK7AK DD DSN=WG01.QASE.WDK7AK,DISP=SHR                                   
//SIU.WDK7BK DD DSN=WG01.QASE.WDK7BK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:45                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDK7,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDK70SV9                                         
