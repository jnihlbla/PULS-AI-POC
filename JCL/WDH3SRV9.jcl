//WDH3SRV9 JOB (650W0020200WDH3SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDH3CLU                                          
//*                                                                             
//WDH3    EXEC WG01REL,                                                         
//             DBD=WDH3                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(126000,18000),RLSE),                                
//             DCB=BUFNO=10                                                     
//REL.DBORELD1 DD DSN=WG01.UNLO.WDH3K(+0),DISP=SHR                              
//REL.WDH3K DD DSN=WG01.QASE.WDH3K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDH3SRV9                                         
