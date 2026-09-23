//WDA2ULV9 JOB (640W0020200WDA2ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDA2 EXEC WG01UNL,                                                            
//             DBD=WDA2                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDA2V(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            DCB=BUFNO=10,                                                     
//            SPACE=(8192,(210000,9000),RLSE),                                  
//            MGMTCLAS=DEL2BKPC                                                 
//UNL.WDA2V DD DSN=WG01.QASE.WDA2V,DISP=SHR                                     
//UNL.W010WDA2 DD DSN=W418.W010V9.W41882(+0),DISP=SHR                           
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:40                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDA2,                                                                 
  FILEFORMAT=LONG,                                                              
  USEREXITLE=W010WDA2,                                                          
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDA2ULV9                                         
//*                                                                             
