//WDD90SV9 JOB (640W0020200WDD90SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDD9V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDD9ACLU),DISP=SHR                           
//*                                                                             
//WDD9    EXEC WG01SIU                                                          
//SIU.WDD9V  DD DSN=WG01.QASE.WDD9V,DISP=SHR                                    
//SIU.WDD9AK DD DSN=WG01.QASE.WDD9AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDD9,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDD90SV9                                         
