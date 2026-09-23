//WDN40SV9 JOB (640W0020200WDN40SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDN4ACLU                                         
//DD1     DD  DSN=WG01.QASE.WDN4V,DISP=SHR                                      
//*                                                                             
//WDN4    EXEC WG01SIU                                                          
//SIU.WDN4V  DD  DSN=WG01.QASE.WDN4V,DISP=SHR                                   
//SIU.WDN4AK DD  DSN=WG01.QASE.WDN4AK,DISP=SHR                                  
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDN4,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDN40SV9                                         
