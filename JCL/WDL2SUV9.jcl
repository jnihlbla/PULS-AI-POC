//WDL2SUV9 JOB (650W0020200WDL2SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL2    EXEC WG01UNL,                                                         
//             DBD=WDL2                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDL2V(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(252000,12600),RLSE),                                
//             MGMTCLAS=DEL2,DATACLAS=MVOL                                      
//UNL.WDL2V DD DSN=WG01.QASE.WDL2V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:45                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDL2,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL2SUV9                                         
//*                                                                             
