//WDM8SUV9 JOB (640W0020200WDM8SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*                                                                              
//WDM8 EXEC WG01UNL,                                                            
//             DBD=WDM8                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDM8K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(2048,(900,90),RLSE),                                       
//            MGMTCLAS=DEL2,DATACLAS=MVOL                                       
//UNL.WDM8K DD DSN=WG01.QASE.WDM8K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDM8,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDM8SUV9                                         
