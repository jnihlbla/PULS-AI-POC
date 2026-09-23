//WDH5SUV9 JOB (650W0020200WDH5SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDH5    EXEC WG01UNL,                                                         
//             DBD=WDH5                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDH5V(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(126000,18000),RLSE),                                
//             MGMTCLAS=DEL2                                                    
//UNL.WDH5V DD DSN=WG01.QASE.WDH5V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:44                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDH5,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDH5SUV9                                         
