//WDL3RLV9 JOB (650W0020200WDL30RV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDL3CLU                                          
//*                                                                             
//WDL3    EXEC WG01REL,                                                         
//             DBD=WDL3                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDL3K(+0),DISP=SHR                              
//REL.WDL3K DD DSN=WG01.QASE.WDL3K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL3RLV9                                         
