//WDD20SV9 JOB (640W0020200WDD20SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDD2V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDD2ACLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDD2BCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDD2CCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDD2DCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDD2ECLU),DISP=SHR                           
//*                                                                             
//WDD2    EXEC WG01SIU                                                          
//SIU.WDD2V  DD DSN=WG01.QASE.WDD2V,DISP=SHR                                    
//SIU.WDD2AK DD DSN=WG01.QASE.WDD2AK,DISP=SHR                                   
//SIU.WDD2BK DD DSN=WG01.QASE.WDD2BK,DISP=SHR                                   
//SIU.WDD2CK DD DSN=WG01.QASE.WDD2CK,DISP=SHR                                   
//SIU.WDD2DK DD DSN=WG01.QASE.WDD2DK,DISP=SHR                                   
//SIU.WDD2EK DD DSN=WG01.QASE.WDD2EK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDD2,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDD20SV9                                         
