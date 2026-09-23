//WDK50RV9 JOB (640W0020200WDK50RV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDK5CLU                                          
//*                                                                             
//WDK5    EXEC WG01REL,                                                         
//             DBD=WDK5                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDK5K(+0),DISP=SHR                              
//REL.WDK5K DD DSN=WG01.QASE.WDK5K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDK50RV9                                         
