//WDE4ULV9 JOB (640W0020200WDE4ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDE4    EXEC WG01UNL,                                                         
//             DBD=WDE4                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDE4V(+1),DISP=(NEW,CATLG,DELETE),              
//             SPACE=(8192,(216000,6300),RLSE),                                 
//             MGMTCLAS=DEL2BKPC                                                
//UNL.WDE4V DD DSN=WG01.QASE.WDE4V,DISP=SHR                                     
//UNL.W010WDE4 DD DSN=W479.W010V9.W479E4(+0),DISP=SHR                           
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:42                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDE4,                                                                 
  FILEFORMAT=LONG,                                                              
  USEREXITLE=W010WDE4,                                                          
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDE4ULV9                                         
