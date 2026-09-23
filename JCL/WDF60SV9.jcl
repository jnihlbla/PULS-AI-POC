//WDF60SV9 JOB (640W0020200WDF60SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1     DD  DSN=WG01.QASE.WDF6V,DISP=SHR                                      
//SYSIN   DD  DSN=W.QASE.CONSTANT(WDF6ACLU),DISP=SHR                            
//*                                                                             
//WDF6    EXEC WG01SIU                                                          
//SIU.WDF6V  DD  DSN=WG01.QASE.WDF6V,DISP=SHR                                   
//SIU.WDF6AK DD  DSN=WG01.QASE.WDF6AK,DISP=SHR                                  
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:43                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDF6,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDF60SV9                                         
