//WDE2ULV9 JOB (640W0020200WDE2ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDE2    EXEC WG01UNL,                                                         
//             DBD=WDE2                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDE2V(+1),DISP=(NEW,CATLG,DELETE),              
//             SPACE=(8192,(36000,6300),RLSE),                                  
//             MGMTCLAS=DEL2                                                    
//UNL.WDE2V DD DSN=WG01.QASE.WDE2V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDE2,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDE2ULV9                                         
