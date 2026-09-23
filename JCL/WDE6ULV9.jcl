//WDE6ULV9 JOB (640W0020200WDE6ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDE6    EXEC WG01UNL,                                                         
//             DBD=WDE6                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDE6V(+1),DISP=(NEW,CATLG,DELETE),              
//             SPACE=(4096,(216000,6300),RLSE),                                 
//             MGMTCLAS=DEL2BKPC                                                
//UNL.WDE6V DD DSN=WG01.QASE.WDE6V,DISP=SHR                                     
//UNL.W010WDE6 DD DSN=W479.W010V9.W479E6(+0),DISP=SHR                           
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDE6,                                                                 
  FILEFORMAT=LONG,                                                              
  USEREXITLE=W010WDE6,                                                          
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDE6ULV9                                         
