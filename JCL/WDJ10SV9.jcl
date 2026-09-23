//WDJ10SV9 JOB (640W0020200WDJ10SV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD  DSN=WG01.QASE.WDJ1V,DISP=SHR                                     
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDJ1ACLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDJ1BCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDJ1CCLU),DISP=SHR                           
//         DD  DSN=W.QASE.CONSTANT(WDJ1DCLU),DISP=SHR                           
//*                                                                             
//WDJ1    EXEC WG01SIU                                                          
//SIU.WDJ1V  DD DSN=WG01.QASE.WDJ1V,DISP=SHR                                    
//SIU.WDJ1AK DD DSN=WG01.QASE.WDJ1AK,DISP=SHR                                   
//SIU.WDJ1BK DD DSN=WG01.QASE.WDJ1BK,DISP=SHR                                   
//SIU.WDJ1CK DD DSN=WG01.QASE.WDJ1CK,DISP=SHR                                   
//SIU.WDJ1DK DD DSN=WG01.QASE.WDJ1DK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:44                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDJ1,ICNEEDED=OFF,                               
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDJ10SV9                                         
