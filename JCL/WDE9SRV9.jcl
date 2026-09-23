//WDE9SRV9 JOB (650W0020200WDE9SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDE9CLU                                          
//*                                                                             
//WDE9    EXEC WG01REL,                                                         
//             DBD=WDE9                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDE9K(+0),DISP=SHR                              
//REL.WDE9K DD DSN=WG01.QASE.WDE9K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDE9SRV9                                         
