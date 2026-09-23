//WDT4RLV9 JOB (650W0020200WDT4RLV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDT4CLU                                          
//*                                                                             
//WDT4    EXEC WG01REL,                                                         
//             DBD=WDT4                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDT4K(+0),DISP=SHR                              
//REL.WDT4K DD DSN=WG01.QASE.WDT4K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDT4RLV9                                         
