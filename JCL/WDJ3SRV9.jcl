//WDJ3SRV9 JOB (650W0020200WDJ3SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*                                                                              
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDJ3CLU                                          
/*                                                                              
//WDJ3    EXEC WG01REL,                                                         
//             DBD=WDJ3                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDJ3K(+0),DISP=SHR                              
//REL.WDJ3K DD DSN=WG01.QASE.WDJ3K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDJ3SRV9                                         
