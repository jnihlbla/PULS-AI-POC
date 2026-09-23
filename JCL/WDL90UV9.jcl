//WDL90UV9 JOB (650W0020200WDL90UV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//       EXEC WG01UNL,                                                          
//             DBD=WDL9                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDL9K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(CYL,(1500,50),RLSE),                                       
//            MGMTCLAS=DEL2BKPC,DATACLAS=MVOL                                   
//UNL.WDL9K DD DSN=WG01.QASE.WDL9K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:46                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDL9,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL90UV9                                         
