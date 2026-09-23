//WDJ20SV9 JOB (650W0020200WDJ20SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDJ2V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDJ2ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDJ2BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDJ2CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDJ2DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDJ2ECLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.WDJ2V DD DSN=WG01.QASE.WDJ2V,DISP=SHR                                     
//SIU.WDJ2AK DD DSN=WG01.QASE.WDJ2AK,DISP=SHR                                   
//SIU.WDJ2BK DD DSN=WG01.QASE.WDJ2BK,DISP=SHR                                   
//SIU.WDJ2CK DD DSN=WG01.QASE.WDJ2CK,DISP=SHR                                   
//SIU.WDJ2DK DD DSN=WG01.QASE.WDJ2DK,DISP=SHR                                   
//SIU.WDJ2EK DD DSN=WG01.QASE.WDJ2EK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:44                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDJ2,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDJ20SV9                                         
