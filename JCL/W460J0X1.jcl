//W460J0X1 JOB (670W4600100W460J0X1,W100),'RTN W460X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* EXPEDITER=&VCOM                                                             
//* COUNTRYX2=&COUNTRYX2                                                        
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W460XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W460.&VCOM..W46001(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(254,(100,10),RLSE),AVGREC=K,                              
//             DCB=(RECFM=VB,LRECL=254),                                        
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//ORDER   EXEC WSOP                                                             
SET VALUE W460X1                                                                
  VCOM(&VCOM)                                                                   
END-SET                                                                         
IF-SYMBOL W460X1 VCOM(W460X1AT)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(AT)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1AU)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(AU)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1BE)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(BE)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1BR)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(BR)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1CA)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(CA)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1CH)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(CH)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1CN)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(CN)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1C1)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(C1)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1DE)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(DE)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1DK)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(DK)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1ES)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(ES)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1FI)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(FI)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1FR)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(FR)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1GB)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(GB)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1IE)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(IE)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1IT)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(IT)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1JP)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(JP)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1KR)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(KR)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1MX)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(MX)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1MY)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(MY)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1NL)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(NL)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1NO)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(NO)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1PL)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(PL)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1PT)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(PT)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1PX)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(PX)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1RU)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(RU)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1SE)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(SE)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1TH)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(TH)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1TR)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(TR)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1TW)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(TW)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1TX)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(TX)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1US)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(US)                                                     
 END-ORDER                                                                      
END-IF                                                                          
IF-SYMBOL W460X1 VCOM(W460X1ZA)                                                 
 ORDER W460S3 SYMBOLS                                                           
  VCOM(&VCOM) COUNTRYX2(ZA)                                                     
 END-ORDER                                                                      
END-IF                                                                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W460J0X1                                         
