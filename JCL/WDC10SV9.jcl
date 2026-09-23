//WDC10SV9 JOB (640W0020200WDC10SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//SYSIN    DD DSN=W.QASE.CONSTANT(WDC1ACLU),DISP=SHR                            
//*                                                                             
//WDC1    EXEC WG01SIU                                                          
//SIU.WDC1V  DD DSN=WG01.QASE.WDC1V,DISP=SHR                                    
//SIU.WDC1AK DD DSN=WG01.QASE.WDC1AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDC1,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDC10SV9                                         
