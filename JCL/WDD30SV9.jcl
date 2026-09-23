//WDD30SV9 JOB (640W0020200WDD30SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDD3V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDD3ACLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDD3BCLU),DISP=SHR                           
//*                                                                             
//WDD3    EXEC WG01SIU                                                          
//SIU.WDD3V  DD DSN=WG01.QASE.WDD3V,DISP=SHR                                    
//SIU.WDD3AK DD DSN=WG01.QASE.WDD3AK,DISP=SHR                                   
//SIU.WDD3BK DD DSN=WG01.QASE.WDD3BK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDD3,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDD30SV9                                         
