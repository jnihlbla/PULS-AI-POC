//WDG80SV9 JOB (640W0020200WDG80SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDG8V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDG8ACLU),DISP=SHR                           
//*                                                                             
//WDG8    EXEC WG01SIU                                                          
//SIU.WDG8V  DD DSN=WG01.QASE.WDG8V,DISP=SHR                                    
//SIU.WDG8AK DD DSN=WG01.QASE.WDG8AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:43                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDG8,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDG80SV9                                         
