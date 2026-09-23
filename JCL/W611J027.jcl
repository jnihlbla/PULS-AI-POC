//W611J027 JOB (640W6110100W611J027,W100),'RTN W611V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P027                                                         
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN  DD  *                                                                
  %RENDATE  'W611.W611V1.W61127(+0)'  'W020.V%AAVV..ARHIST'                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J027                                         
