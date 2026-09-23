//WDG9SRV9 JOB (650W0020200WDG9SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDG9CLU                                          
//WDG9    EXEC WG01REL,                                                         
//             DBD=WDG9                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(180,180),RLSE),                                     
//             DCB=BUFNO=10                                                     
//REL.DBORELD1 DD DSN=WG01.UNLO.WDG9K(+0),DISP=SHR                              
//REL.WDG9K DD DSN=WG01.QASE.WDG9K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDG9SRV9                                         
