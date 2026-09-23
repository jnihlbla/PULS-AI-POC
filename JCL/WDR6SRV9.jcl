//WDR6SRV9 JOB (650W0020200WDR6SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDR6CLU                                          
//*                                                                             
//WDR6    EXEC WG01REL,                                                         
//             DBD=WDR6                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDR6K(+0),DISP=SHR                              
//REL.WDR6K DD DSN=WG01.QASE.WDR6K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR6SRV9                                         
