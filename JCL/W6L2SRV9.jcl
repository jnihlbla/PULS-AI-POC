//W6L2SRV9 JOB (650W0020200W6L2SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=W6L2CLU                                          
//*                                                                             
//W6L2    EXEC WG01REL,                                                         
//             DBD=W6L2                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.W6L2K(+0),DISP=SHR                              
//REL.W6L2K DD DSN=WG01.QASE.W6L2K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6L2SRV9                                         
