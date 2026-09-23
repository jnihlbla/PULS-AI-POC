//WDA7SUV9 JOB (650W0020200WDA7SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//       EXEC WG01UNL,                                                          
//             DBD=WDA7                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDA7K(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(18000,1800),RLSE),                                  
//             MGMTCLAS=DEL2BKPC                                                
//UNL.WDA7K DD DSN=WG01.QASE.WDA7K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDA7,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDA7SUV9                                         
