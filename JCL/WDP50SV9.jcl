//WDP50SV9 JOB (640W0020200WDP50SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDP5V,DISP=SHR                                      
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDP5ACLU),DISP=SHR                           
//*                                                                             
//WDP5    EXEC WG01SIU                                                          
//SIU.WDP5V  DD DSN=WG01.QASE.WDP5V,DISP=SHR                                    
//SIU.WDP5AK DD DSN=WG01.QASE.WDP5AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDP5,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDP50SV9                                         
