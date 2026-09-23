//WDA3ULV9 JOB (640W0020200WDA3ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDA3 EXEC WG01UNL,                                                            
//             DBD=WDA3                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDA3V(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            DCB=BUFNO=10,                                                     
//            SPACE=(4096,(720,900),RLSE),                                      
//            MGMTCLAS=DEL2BKPC                                                 
//UNL.WDA3V DD DSN=WG01.QASE.WDA3V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDA3,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDA3ULV9                                         
//*                                                                             
