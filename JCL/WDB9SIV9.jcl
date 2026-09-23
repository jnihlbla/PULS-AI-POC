//WDB9SIV9 JOB (640W0020200WDB9SIV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDB9K,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDB9ACLU),DISP=SHR                           
//*                                                                             
//WDB9    EXEC WG01SIU                                                          
//SIU.WDB9K  DD DSN=WG01.QASE.WDB9K,DISP=SHR                                    
//SIU.WDB9AK DD DSN=WG01.QASE.WDB9AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDB9,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDB9SIV9                                         
