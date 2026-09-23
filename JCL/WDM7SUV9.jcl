//WDM7SUV9 JOB (640W0020200WDM7SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDM7 EXEC WG01UNL,                                                            
//             DBD=WDM7                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDM7K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(4096,(2800,900),RLSE),                                     
//            MGMTCLAS=DEL2,DATACLAS=MVOL                                       
//UNL.WDM7K DD DSN=WG01.QASE.WDM7K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:47                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDM7,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDM7SUV9                                         
