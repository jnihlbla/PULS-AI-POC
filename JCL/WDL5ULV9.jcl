//WDL5ULV9 JOB (640W0020200WDL5ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL5    EXEC WG01UNL,                                                         
//             DBD=WDL5                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDL5V(+1),DISP=(NEW,CATLG,DELETE),              
//             SPACE=(16384,(90000,9000),RLSE),                                 
//             MGMTCLAS=DEL2                                                    
//UNL.WDL5V DD DSN=WG01.QASE.WDL5V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDL5,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL5ULV9                                         
