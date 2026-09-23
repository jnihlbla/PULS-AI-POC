//WDL8ULV9 JOB (640W0020200WDL8ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL8 EXEC WG01UNL,                                                            
//             DBD=WDL8                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDL8V(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            SPACE=(16384,(162000,9000),RLSE),                                 
//            MGMTCLAS=DEL2BKPC                                                 
//UNL.WDL8V DD DSN=WG01.QASE.WDL8V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:46                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDL8,                                                                 
  FILEFORMAT=LONG,                                                              
  USEREXITLE=W010WDL8,                                                          
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL8ULV9                                         
//*                                                                             
