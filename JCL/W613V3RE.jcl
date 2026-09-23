//W613V3RE JOB (640W6130100W613V3RE,W100),'RTN W613V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN DD  *                                                                 
  %RENDATE  'W613.W613V3.W61323(+0)' 'WXTR.V%AAVV..W61323'                      
  %RENDATE  'W613.W613V3.W61324(+0)' 'WXTR.V%AAVV..W61324'                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W613V3RE                                         
