//WDQ3SUV9 JOB (650W0020200WDQ3SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDQ3 EXEC WG01UNL,                                                            
//             DBD=WDQ3                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDQ3K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(4096,(35000,1800),RLSE),                                   
//            MGMTCLAS=DEL2                                                     
//UNL.WDQ3K DD DSN=WG01.QASE.WDQ3K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:48                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDQ3,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDQ3SUV9                                         
