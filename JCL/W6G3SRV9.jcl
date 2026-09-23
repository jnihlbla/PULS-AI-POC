//W6G3SRV9 JOB (650W0020200W6G3SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=W6G3CLU                                          
//*                                                                             
//W6G3    EXEC WG01REL,                                                         
//             DBD=W6G3                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.W6G3K(+0),DISP=SHR                              
//REL.W6G3K DD DSN=WG01.QASE.W6G3K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6G3SRV9                                         
