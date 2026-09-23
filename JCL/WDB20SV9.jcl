//WDB20SV9 JOB (640W0020200WDB20SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDB2K,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDB2ACLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDB2BCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDB2CCLU),DISP=SHR                           
//*                                                                             
//WDB2    EXEC WG01SIU                                                          
//SIU.WDB2K  DD DSN=WG01.QASE.WDB2K,DISP=SHR                                    
//SIU.WDB2AK DD DSN=WG01.QASE.WDB2AK,DISP=SHR                                   
//SIU.WDB2BK DD DSN=WG01.QASE.WDB2BK,DISP=SHR                                   
//SIU.WDB2CK DD DSN=WG01.QASE.WDB2CK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDB2,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDB20SV9                                         
