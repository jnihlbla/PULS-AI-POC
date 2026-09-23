//WDF8SIV9 JOB (640W0020200WDF8SIV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//SYSIN    DD DSN=W.QASE.CONSTANT(WDF8ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDF8BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDF8CCLU),DISP=SHR                            
//*                                                                             
//WDF8    EXEC WG01SIU                                                          
//SIU.WDF8V  DD DSN=WG01.QASE.WDF8V,DISP=SHR                                    
//SIU.WDF8AK DD DSN=WG01.QASE.WDF8AK,DISP=SHR                                   
//SIU.WDF8BK DD DSN=WG01.QASE.WDF8BK,DISP=SHR                                   
//SIU.WDF8CK DD DSN=WG01.QASE.WDF8CK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:43                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDF8,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDF8SIV9                                         
