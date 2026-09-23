//WDL90RV9 JOB (650W0020200WDL90RV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDL9CLU                                          
//*                                                                             
//WDL9    EXEC WG01REL,                                                         
//             DBD=WDL9                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDL9K(+0),DISP=SHR                              
//REL.WDL9K DD DSN=WG01.QASE.WDL9K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL90RV9                                         
