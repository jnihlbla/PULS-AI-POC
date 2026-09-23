//WDN60SV9 JOB (640W0020200WDN60SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDN6ACLU                                         
//DD1     DD  DSN=WG01.QASE.WDN6V,DISP=SHR                                      
//*                                                                             
//WDN6    EXEC WG01SIU                                                          
//SIU.WDN6V  DD  DSN=WG01.QASE.WDN6V,DISP=SHR                                   
//SIU.WDN6AK DD  DSN=WG01.QASE.WDN6AK,DISP=SHR                                  
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDN6,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDN60SV9                                         
