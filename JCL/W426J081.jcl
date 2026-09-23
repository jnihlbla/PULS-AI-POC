//W426J081 JOB (640W4260100W426J081,W100),'RTN W426V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P081                                                         
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN  DD  *                                                                
  %RENDATE  'W426.W426V4.W42681(+0)'  'WXTR.V%AAVV..WXTRK4'                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426J081                                         
