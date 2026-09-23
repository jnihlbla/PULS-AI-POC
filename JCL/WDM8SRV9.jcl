//WDM8SRV9 JOB (640W0020200WDM8SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*                                                                              
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDM8CLU                                          
//*                                                                             
//WDM8    EXEC WG01REL,                                                         
//             DBD=WDM8                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDM8K(+0),DISP=SHR                              
//REL.WDM8K DD DSN=WG01.QASE.WDM8K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDM8SRV9                                         
//*                                                                             
