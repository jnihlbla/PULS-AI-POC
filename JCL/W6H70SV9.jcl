//W6H70SV9 JOB (640W0020200W6H70SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.W6H7V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(W6H7ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6H7BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(W6H7CCLU),DISP=SHR                            
//*                                                                             
//W6H7    EXEC WG01SIU                                                          
//SIU.W6H7V  DD  DSN=WG01.QASE.W6H7V,DISP=SHR                                   
//SIU.W6H7AK DD  DSN=WG01.QASE.W6H7AK,DISP=SHR                                  
//SIU.W6H7BK DD  DSN=WG01.QASE.W6H7BK,DISP=SHR                                  
//SIU.W6H7CK DD  DSN=WG01.QASE.W6H7CK,DISP=SHR                                  
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:44:06                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=W6H7,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W6H70SV9                                         
