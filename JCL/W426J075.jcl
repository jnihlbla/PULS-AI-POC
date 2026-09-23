//W426J075 JOB (640W4260100W426J075,W100),'RTN W426V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P075                                                         
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN  DD  *                                                                
  %RENDATE  'W426.W426V4.W42675(+0)'  'WXTR.W426V4.WXTRK5(+1)'                  
  %RENDATE  'W426.W426V4.W42677(+0)'  'WXTR.W426V4.WXTRK7(+1)'                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426J075                                         
