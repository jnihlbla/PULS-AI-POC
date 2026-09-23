//WDR80UV9 JOB (650W0020200WDR80UV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//       EXEC WG01UNL,                                                          
//             DBD=WDR8                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDR8K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(CYL,(10,10),RLSE),                                         
//            MGMTCLAS=DEL2BKPC                                                 
//UNL.WDR8K DD DSN=WG01.QASE.WDR8K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:49                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDR8,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR80UV9                                         
