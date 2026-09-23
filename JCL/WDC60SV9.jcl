//WDC60SV9 JOB (640W0020200WDC60SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDC6ACLU                                         
//DD1     DD  DSN=WG01.QASE.WDC6V,DISP=SHR                                      
//*                                                                             
//WDC6    EXEC WG01SIU                                                          
//SIU.WDC6V  DD  DSN=WG01.QASE.WDC6V,DISP=SHR                                   
//SIU.WDC6AK DD  DSN=WG01.QASE.WDC6AK,DISP=SHR                                  
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDA2,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDC60SV9                                         
