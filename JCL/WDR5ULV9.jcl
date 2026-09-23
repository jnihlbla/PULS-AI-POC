//WDR5ULV9 JOB (640W0020200WDR5ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR5 EXEC WG01UNL,                                                            
//             DBD=WDR5                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDR5V(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            DCB=BUFNO=10,                                                     
//            SPACE=(8192,(1080,900),RLSE),                                     
//            MGMTCLAS=DEL2BKPC                                                 
//UNL.WDR5V DD DSN=WG01.QASE.WDR5V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:49                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDR5,                                                                 
  FILEFORMAT=LONG,                                                              
  USEREXITLE=W010WDR5,                                                          
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR5ULV9                                         
//*                                                                             
