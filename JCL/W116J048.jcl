//W116J048 JOB (640W1160100W116J048,W100),'RTN W116S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* VCOM     =&VCOM                                                             
//* COUNTRYX2=&COUNTRYX2                                                        
//W116    EXEC W116P048                                                         
//*                                                                             
//SYSINPUT DD *                                                                 
&COUNTRYX2                                                                      
//W11648.SYSUDUMP DD SYSOUT=*                                                   
//W11648.W11648D2 DD MGMTCLAS=DEL20                                             
//SOPSET  EXEC WSOP                                                             
SET VALUE W116S1                                                                
 COUNTRYX2(&COUNTRYX2)                                                          
END-SET                                                                         
IF-SYMBOL W116S1 COUNTRYX2(BE)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3BE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(DE)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3DE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(DK)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3DK)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(ES)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3ES)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(GB)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3GB)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(MY)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3MY)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(NL)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3NL)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(NO)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3NO)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(PL)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3PL)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(SE)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3SE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(TH)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3TH)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(AT)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3AT)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(FI)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3FI)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(IT)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3IT)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(FR)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3FR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(TW)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3TW)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(CH)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3CH)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(IE)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3IE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(BR)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3BR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(MX)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3MX)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(TR)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3TR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(TX)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3TX)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(RU)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3RU)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(ZA)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3ZA)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(PT)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3PX)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(CZ)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3CZ)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W116S1 COUNTRYX2(HU)                                                  
SET VALUE W116S1                                                                
  VCOM2(W116Z3HU)                                                               
END-SET                                                                         
END-IF                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J048                                         
