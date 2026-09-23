//WDE3SUV9 JOB (640W0020200WDE3SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDE3 EXEC WG01UNL,                                                            
//             DBD=WDE3                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDE3K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(4096,(180,180),RLSE),                                      
//            MGMTCLAS=DEL2                                                     
//UNL.WDE3K DD DSN=WG01.QASE.WDE3K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDE3,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDE3SUV9                                         
