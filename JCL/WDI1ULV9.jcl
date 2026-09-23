//WDI1ULV9 JOB (640W0020200WDI1ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDI1 EXEC WG01UNL,                                                            
//            DBD=WDI1                                                          
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDI1V(+1),  -LIM(10)                            
//            DISP=(NEW,CATLG,DELETE),                                          
//            DCB=(BUFNO=10),                                                   
//            SPACE=(4096,(36000,1800),RLSE),                                   
//            MGMTCLAS=DEL2BKPC                                                 
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:44                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDI1,                                                                 
  FILEFORMAT=LONG,                                                              
  USEREXITLE=W010WDI1,                                                          
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//UNL.WDI1V DD DSN=WG01.QASE.WDI1V,DISP=SHR,                                    
//             DCB=BUFNO=10                                                     
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDI1ULV9                                         
