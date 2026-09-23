//WDD6RLV9 JOB (640W0020200WDD6LRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDD6CLU                                          
//*                                                                             
//WDD6    EXEC WG01REL,                                                         
//             DBD=WDD6                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDD6K(+0),DISP=SHR                              
//REL.WDD6K DD DSN=WG01.QASE.WDD6K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDD6RLV9                                         
