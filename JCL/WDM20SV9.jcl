//WDM20SV9 JOB (640W0020200WDM20SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDM2V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDM2ACLU),DISP=SHR                           
//*                                                                             
//WDM2    EXEC WG01SIU                                                          
//SIU.WDM2V  DD DSN=WG01.QASE.WDM2V,DISP=SHR                                    
//SIU.WDM2AK DD DSN=WG01.QASE.WDM2AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDM2,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDM20SV9                                         
