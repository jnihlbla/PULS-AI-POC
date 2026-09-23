//WDD6ULV9 JOB (650W0020200WDD6ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDD6    EXEC WG01UNL,                                                         
//             DBD=WDD6                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDD6K(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(1800,180),RLSE),                                    
//             MGMTCLAS=DEL2BKPC                                                
//UNL.WDD6K DD DSN=WG01.QASE.WDD6K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDD6,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDD6ULV9                                         
