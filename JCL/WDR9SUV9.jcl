//WDR9SUV9 JOB (650W0020200WDR9SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR9    EXEC WG01UNL,                                                         
//             DBD=WDR9,COND.ABEND=(4,GE,UNL)                                   
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDR9K(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(126000,18000),RLSE),                                
//             MGMTCLAS=DEL2                                                    
//UNL.WDR9K DD DSN=WG01.QASE.WDR9K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:49                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDR9,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR9SUV9                                         
