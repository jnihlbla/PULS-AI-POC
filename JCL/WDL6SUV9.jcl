//WDL6SUV9 JOB (650W0020200WDL6SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL6    EXEC WG01UNL,                                                         
//             DBD=WDL6                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDL6V(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(8192,(250000,27000),RLSE),                                
//             MGMTCLAS=DEL2,DATACLAS=MVOL                                      
//UNL.WDL6V DD DSN=WG01.QASE.WDL6V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:45                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDL6,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL6SUV9                                         
//*                                                                             
