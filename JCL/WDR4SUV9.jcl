//WDR4SUV9 JOB (650W0020200WDR4SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR4    EXEC WG01UNL,                                                         
//             DBD=WDR4,COND.ABEND=(4,GE,UNL)                                   
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDR4V(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(8192,(3600,360),RLSE),                                    
//             MGMTCLAS=DEL2,DATACLAS=MVOL                                      
//UNL.WDR4V DD DSN=WG01.QASE.WDR4V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:49                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDR4,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR4SUV9                                         
//*                                                                             
