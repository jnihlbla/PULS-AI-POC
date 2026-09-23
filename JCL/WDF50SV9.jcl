//WDF50SV9 JOB (640W0020200WDF50SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1     DD  DSN=WG01.QASE.WDF5V,DISP=SHR                                      
//SYSIN   DD  DSN=W.QASE.CONSTANT(WDF5ACLU),DISP=SHR                            
//        DD  DSN=W.QASE.CONSTANT(WDF5BCLU),DISP=SHR                            
//*                                                                             
//WDF5    EXEC WG01SIU                                                          
//SIU.WDF5V  DD  DSN=WG01.QASE.WDF5V,DISP=SHR                                   
//SIU.WDF5AK DD  DSN=WG01.QASE.WDF5AK,DISP=SHR                                  
//SIU.WDF5BK DD  DSN=WG01.QASE.WDF5BK,DISP=SHR                                  
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:43                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDF5,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDF50SV9                                         
