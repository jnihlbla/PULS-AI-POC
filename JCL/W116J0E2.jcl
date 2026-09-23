//W116J0E2 JOB (640W1160100W116J0E2,W100),'RTN W116E2',                         
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
W116.*.W11694 FB 210                                                            
//*                                                                             
//ORDER   EXEC WSOP                                                             
//W98022D1 DD  *                                                                
  ORDER W116SG SYMBOLS                                                          
//   DD DSN=&&SOPPARM,DISP=(OLD,DELETE)                                         
//   DD *                                                                       
  END-ORDER                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W116J0E2                                         
