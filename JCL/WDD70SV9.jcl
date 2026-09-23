//WDD70SV9 JOB (640W0020200WDD70SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDD7ACLU                                         
//DD1     DD  DSN=WG01.QASE.WDD7V,DISP=SHR                                      
//*                                                                             
//WDD7    EXEC WG01SIU                                                          
//SIU.WDD7V  DD  DSN=WG01.QASE.WDD7V,DISP=SHR                                   
//SIU.WDD7AK DD  DSN=WG01.QASE.WDD7AK,DISP=SHR                                  
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDD7,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDD70SV9                                         
