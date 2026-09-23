//WDI2ULV9 JOB (640W0020200WDI2ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDI2 EXEC WG01UNL,                                                            
//            DBD=WDI2                                                          
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDI2V(+1),  -LIM(10)                            
//            DISP=(NEW,CATLG,DELETE),                                          
//            DCB=(BUFNO=10),                                                   
//            SPACE=(4096,(27000,1800),RLSE),                                   
//            MGMTCLAS=DEL2BKPC                                                 
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:44                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDI2,                                                                 
  FILEFORMAT=LONG,                                                              
  USEREXITLE=W010WDI2,                                                          
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//UNL.WDI2V DD DSN=WG01.QASE.WDI2V,DISP=SHR,                                    
//             DCB=BUFNO=10                                                     
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDI2ULV9                                         
