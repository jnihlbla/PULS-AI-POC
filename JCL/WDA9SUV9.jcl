//WDA9SUV9 JOB (650W0020200WDA9SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDA9    EXEC WG01UNL,                                                         
//             DBD=WDA9,COND.ABEND=(4,GE,UNL)                                   
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDA9V(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(5040,360),RLSE),                                    
//             MGMTCLAS=DEL2,DATACLAS=MVOL                                      
//UNL.WDA9V DD DSN=WG01.QASE.WDA9V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDA9,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDA9SUV9                                         
//*                                                                             
