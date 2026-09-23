//WDL1SUV9 JOB (650W4790100WDL1SUY1,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL1   EXEC WG01UNL,                                                          
//             DBD=WDL1                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDL1V(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//*           SPACE=(2048,(510300,6300),RLSE),                                  
//            DATACLAS=PSEN,                                                    
//            MGMTCLAS=DEL2                                                     
//UNL.WDL1V DD DSN=WG01.QASE.WDL1V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:45                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDL1,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL1SUV9                                         
