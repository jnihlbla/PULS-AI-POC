//WDD4ULV9 JOB (650W0020200WDD4ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDD4    EXEC WG01UNL,                                                         
//             DBD=WDD4                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDD4K(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(1800,180),RLSE),                                    
//             MGMTCLAS=DEL2BKPC                                                
//UNL.WDD4K DD DSN=WG01.QASE.WDD4K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:41                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDD4,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDD4ULV9                                         
