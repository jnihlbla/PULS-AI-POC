//WDJ8SUV9 JOB (650W0020200WDJ8SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDJ8 EXEC WG01UNL,                                                            
//             DBD=WDJ8                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDJ8K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//*           SPACE=(4096,(1800,180),RLSE),                                     
//            DATACLAS=PSEN,                                                    
//            MGMTCLAS=DEL2                                                     
//UNL.WDJ8K DD DSN=WG01.QASE.WDJ8K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:45                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDJ8,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDJ8SUV9                                         
