//W612JREN JOB (640W6120100W612J012,W100),'RTN W612V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN DD  *                                                                 
  %RENDATE  'W612.W612V1.W61212(+0)' 'WXTR.V&YYWW..W61212'                      
  %RENDATE  'W612.W612V1.W61214(+0)' 'WXTR.V&YYWW..W61214'                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612JREN                                         
