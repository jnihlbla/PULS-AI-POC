//W116J087 JOB (640W1160100W116J087,W100),'RTN W116SC',                         
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
SET VALUE W116SC                                                                
 VCOM(&VCOM)                                                                    
END-SET                                                                         
IF-SYMBOL W116SC VCOM(W116X1DK)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4DK)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1AT)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4AT)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1AU)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4AU)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1BE)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4BE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1BR)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4BR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1CA)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4CA)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1CH)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4CH)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1DE)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4DE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1ES)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4ES)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1FI)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4FI)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1FR)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4FR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1GB)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4GB)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1IE)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4IE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1IT)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4IT)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1JP)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4JP)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1KR)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4KR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1MX)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4MX)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1MY)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4MY)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1NL)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4NL)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1NO)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4NO)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1PL)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4PL)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1PT)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4PT)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1RU)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4RU)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1SE)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4SE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1TH)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4TH)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1TR)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4TR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1TW)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4TW)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1US)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4US)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1ZA)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4ZA)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1C1)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4C1)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1IN)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4IN)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1CZ)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4CZ)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116SC VCOM(W116X1HU)                                                 
SET VALUE W116SC                                                                
  VCOM2(W116Z4HU)                                                               
END-SET                                                                         
END-IF                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J087                                         
