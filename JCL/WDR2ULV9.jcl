//WDR2ULV9 JOB (640W0020200WDR2ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR2    EXEC WG01UNL,                                                         
//             DBD=WDR2                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDR2V(+1),DISP=(NEW,CATLG,DELETE),              
//             SPACE=(8192,(6300,90),RLSE),                                     
//             MGMTCLAS=DEL2BKPC                                                
//UNL.WDR2V DD DSN=WG01.QASE.WDR2V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:49                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDR2,                                                                 
  FILEFORMAT=LONG,                                                              
  USEREXITLE=W010WDR2,                                                          
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR2ULV9                                         
