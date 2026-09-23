//WXTRV2RE JOB (650W0001000WXTRV2RE,W100),'RTN WXTRV2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN  DD  *                                                                
  %RENDATE  'WXTR.WXTRV2.WXTR01(+0)' 'WXTR.V&YYWW..PACKRAD'                     
//*                                                                             
//RENAME2 EXEC W001PTSO                                                         
//SYSTSIN  DD  *                                                                
  %RENDATE  'WXTR.WXTRV2.WXTRA6(+0)' 'WXTR.V&YYWW..FAKTRADX'                    
//*                                                                             
//FREE    EXEC WFREE,NAME=WXTRV2,MAXRC=8                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRV2RE                                         
