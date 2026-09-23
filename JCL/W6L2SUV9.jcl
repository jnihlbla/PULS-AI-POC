//W6L2SUV9 JOB (650W0020200W6L2SUV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W6L2 EXEC WG01UNL,                                                            
//             DBD=W6L2                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.W6L2K(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(4096,(54000,19000),RLSE),                                  
//            MGMTCLAS=DEL2,DATACLAS=MVOL                                       
//UNL.W6L2K DD DSN=WG01.QASE.W6L2K,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:44:06                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=W6L2,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6L2SUV9                                         
