//WDN70SV9 JOB (640W0020200WDN70SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDN7ACLU                                         
//DD1     DD  DSN=WG01.QASE.WDN7V,DISP=SHR                                      
//*                                                                             
//WDN7    EXEC WG01SIU                                                          
//SIU.WDN7V  DD  DSN=WG01.QASE.WDN7V,DISP=SHR                                   
//SIU.WDN7AK DD  DSN=WG01.QASE.WDN7AK,DISP=SHR                                  
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDN7,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDN70SV9                                         
