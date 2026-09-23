//WDN50SV9 JOB (640W0020200WDN50SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDN5V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDN5ACLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDN5BCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDN5CCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDN5DCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDN5ECLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDN5FCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDN5GCLU),DISP=SHR                           
//*                                                                             
//WDN5    EXEC WG01SIU                                                          
//SIU.WDN5V  DD DSN=WG01.QASE.WDN5V,DISP=SHR                                    
//SIU.WDN5AK DD DSN=WG01.QASE.WDN5AK,DISP=SHR                                   
//SIU.WDN5BK DD DSN=WG01.QASE.WDN5BK,DISP=SHR                                   
//SIU.WDN5CK DD DSN=WG01.QASE.WDN5CK,DISP=SHR                                   
//SIU.WDN5DK DD DSN=WG01.QASE.WDN5DK,DISP=SHR                                   
//SIU.WDN5EK DD DSN=WG01.QASE.WDN5EK,DISP=SHR                                   
//SIU.WDN5FK DD DSN=WG01.QASE.WDN5FK,DISP=SHR                                   
//SIU.WDN5GK DD DSN=WG01.QASE.WDN5GK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDN5,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDN50SV9                                         
