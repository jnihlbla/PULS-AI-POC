//WDL1SRV9 JOB (650W4790100WDL1SRY1,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL1    EXEC WG01REL,                                                         
//             DBD=WDL1                                                         
//REL.DBORELD1 DD DSN=WG01.UNLO.WDL1V(+0),DISP=SHR                              
//REL.WDL1V DD DSN=WG01.QASE.WDL1V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL1SRV9                                         
