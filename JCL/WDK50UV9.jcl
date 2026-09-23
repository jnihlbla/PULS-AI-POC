//WDK50UV9 JOB (640W0020200WDK50UV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//       EXEC WG01UNL,                                                          
//             DBD=WDK5                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDK5K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(4096,(180,180),RLSE),                                      
//            MGMTCLAS=DEL2                                                     
//UNL.WDK5K DD DSN=WG01.QASE.WDK5K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:45                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDK5,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDK50UV9                                         
