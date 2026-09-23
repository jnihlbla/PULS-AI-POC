//W613J020 JOB (640W6130100W613J020,W100),'RTN W613V5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W613    EXEC W613P020                                                         
//*                                                                             
//*                                                                             
//RENAME EXEC W001PTSO                                                          
//SYSIN DD  *                                                                   
  %RENDATE 'W613.W613V5.W61320(+0)' 'WXTR.V%AAVV..W61320'                       
//*  WXTR.VYYVV.W61320                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W613J020                                         
