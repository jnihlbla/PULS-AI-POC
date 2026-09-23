//WDF2SIV9 JOB (640W0020200WDF2SIV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//SYSIN    DD DSN=W.QASE.CONSTANT(WDF2ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDF2BCLU),DISP=SHR                            
//*                                                                             
//WDF2    EXEC WG01SIU                                                          
//SIU.WDF2V  DD DSN=WG01.QASE.WDF2V,DISP=SHR                                    
//SIU.WDF2AK DD DSN=WG01.QASE.WDF2AK,DISP=SHR                                   
//SIU.WDF2BK DD DSN=WG01.QASE.WDF2BK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:43                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDF2,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDF2SIV9                                         
