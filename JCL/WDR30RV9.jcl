//WDR30RV9 JOB (650W0020200WDR30RV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDR3CLU                                          
//*                                                                             
//WDR3    EXEC WG01REL,                                                         
//             DBD=WDR3                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDR3K(+0),DISP=SHR                              
//REL.WDR3K DD DSN=WG01.QASE.WDR3K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR30RV9                                         
