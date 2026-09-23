//W6H60SV9 JOB (640W0020200W6H60SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.W6H6V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(W6H6ACLU),DISP=SHR                           
//*                                                                             
//W6H6    EXEC WG01SIU                                                          
//SIU.W6H6V  DD DSN=WG01.QASE.W6H6V,DISP=SHR                                    
//SIU.W6H6AK DD DSN=WG01.QASE.W6H6AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:44:06                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=W6H6,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W6H60SV9                                         
