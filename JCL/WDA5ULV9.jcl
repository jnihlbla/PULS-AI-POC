//WDA5ULV9 JOB (650W0020200WDA5ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDA5    EXEC WG01UNL,                                                         
//             DBD=WDA5                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDA5K(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(18000,1800),RLSE),                                  
//             MGMTCLAS=DEL2BKPC                                                
//UNL.WDA5K DD DSN=WG01.QASE.WDA5K,DISP=SHR                                     
//UNL.W010WDA5 DD DSN=W479.W010V9.W479A5(+0),DISP=SHR                           
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDA5,                                                                 
  FILEFORMAT=LONG,                                                              
  USEREXITLE=W010WDA5,                                                          
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDA5ULV9                                         
