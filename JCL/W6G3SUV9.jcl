//W6G3SUV9 JOB (650W0020200W6G3SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W6G3 EXEC WG01UNL,                                                            
//             DBD=W6G3                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.W6G3K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(4096,(5500,550),RLSE),                                     
//            MGMTCLAS=DEL2                                                     
//UNL.W6G3K DD DSN=WG01.QASE.W6G3K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:44:06                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=W6G3,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6G3SUV9                                         
