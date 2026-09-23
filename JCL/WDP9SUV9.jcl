//WDP9SUV9 JOB (650W0020200WDP9SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDP9    EXEC WG01UNL,                                                         
//             DBD=WDP9                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDP9K(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(1800,1800),RLSE),                                   
//             MGMTCLAS=DEL2BKPC,DATACLAS=MVOL                                  
//UNL.WDP9K DD DSN=WG01.QASE.WDP9K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:48                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDP9,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDP9SUV9                                         
