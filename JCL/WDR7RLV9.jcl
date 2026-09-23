//WDR7RLV9 JOB (650W0020200WDR7RLV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDR7CLU                                          
//*                                                                             
//WDR7    EXEC WG01REL,                                                         
//             DBD=WDR7,COND.ABEND=(4,GE,REL)                                   
//REL.DBORELD1 DD DSN=WG01.UNLO.WDR7K(+0),DISP=SHR                              
//REL.WDR7K DD DSN=WG01.QASE.WDR7K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR7RLV9                                         
