//W116J187 JOB (640W1160100W116J187,W100),'RTN W116SB',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W116    EXEC W116P087                                                         
//*                                                                             
//W11687.SYSINPUT DD *                                                          
&VCOM                                                                           
//*                                                                             
//SOPSET  EXEC WSOP                                                             
SET VALUE W116SB                                                                
 VCOM(&VCOM)                                                                    
END-SET                                                                         
IF-SYMBOL W116SB VCOM(W116X1M0)                                                 
SET VALUE W116SB                                                                
  VCOM2(W116Z4M0)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SB VCOM(W116X1M1)                                                 
SET VALUE W116SB                                                                
  VCOM2(W116Z4M1)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SB VCOM(W116X1M2)                                                 
SET VALUE W116SB                                                                
  VCOM2(W116Z4M2)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SB VCOM(W116X1M3)                                                 
SET VALUE W116SB                                                                
  VCOM2(W116Z4M3)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SB VCOM(W116X1M5)                                                 
SET VALUE W116SB                                                                
  VCOM2(W116Z4M5)                                                               
END-SET                                                                         
END-IF                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J187                                         
