//WDC7SIV9 JOB (640W0020200WDC7SIV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//SYSIN    DD DSN=W.QASE.CONSTANT(WDC7ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDC7BCLU),DISP=SHR                            
//*                                                                             
//WDC7    EXEC WG01SIU                                                          
//SIU.WDC7V  DD DSN=WG01.QASE.WDC7V,DISP=SHR                                    
//SIU.WDC7AK DD DSN=WG01.QASE.WDC7AK,DISP=SHR                                   
//SIU.WDC7BK DD DSN=WG01.QASE.WDC7BK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDC7,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDC7SIV9                                         
