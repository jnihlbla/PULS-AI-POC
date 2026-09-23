//WDH1ULV9 JOB (640W0020200WDH1ULV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDH1 EXEC WG01UNL,                                                            
//             DBD=WDH1                                                         
//UNL.DBOUNLD1 DD DSN=WG01.UNLO.WDH1V(+1),                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            DCB=BUFNO=10,                                                     
//            SPACE=(2048,(1980,630),RLSE),                                     
//            MGMTCLAS=DEL2BKPC                                                 
//UNL.WDH1V DD DSN=WG01.QASE.WDH1V,DISP=SHR                                     
//UNL.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:43                             
                                                                                
  FUNCTION=UNLOAD,                                                              
  DBDNAME=WDH1,                                                                 
  FILEFORMAT=LONG,                                                              
  ZIIP=YES,                                                                     
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//UNL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDH1ULV9                                         
//*                                                                             
