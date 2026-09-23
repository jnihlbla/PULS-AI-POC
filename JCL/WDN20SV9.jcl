//WDN20SV9 JOB (640W0020200WDN20SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDN2ACLU                                         
//DD1     DD  DSN=WG01.QASE.WDN2V,DISP=SHR                                      
//*                                                                             
//WDN2    EXEC WG01SIU                                                          
//SIU.WDN2V  DD  DSN=WG01.QASE.WDN2V,DISP=SHR                                   
//SIU.WDN2AK DD  DSN=WG01.QASE.WDN2AK,DISP=SHR                                  
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDN2,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDN20SV9                                         
