//W116J0E1 JOB (640W1160100W116J0E1,W100),'RTN W116E1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//VCOM EXEC W016RECV                                                            
&VCOM                                                                           
W116.*.W11601 FB 80                                                             
//*                                                                             
//ORDER   EXEC WSOP                                                             
//W98022D1 DD  *                                                                
  ORDER W116J180 SYMBOLS                                                        
//   DD DSN=&&SOPPARM,DISP=(OLD,DELETE)                                         
//   DD *                                                                       
  END-ORDER                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W116J0E1                                         
