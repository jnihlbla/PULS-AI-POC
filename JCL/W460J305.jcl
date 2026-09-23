//W460J305 JOB (540W4600100W460J305,W100),'RTN W460S3',                         
//             USER=?,PASSWORD=?,                                               
//        CLASS=L                                                               
/*JOBPARM TIME=1,LINES=50,FORMS=1800,LINECT=0                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL W4608S,EXC                                                               
//*                                                                             
//* EXPEDITER=&VCOM                                                             
//* COUNTRYX2=&COUNTRYX2                                                        
//*                                                                             
//W460    EXEC W460P305                                                         
//*                                                                             
//SOPSET  EXEC WSOP                                                             
SET VALUE W460S3                                                                
 COUNTRYX2(&COUNTRYX2)                                                          
END-SET                                                                         
IF-SYMBOL W460S3 COUNTRYX2(CH)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1CH)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(DE)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1DE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(TW)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1TW)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(BE)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1BE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(NL)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1NL)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(TH)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1TH)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(AU)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1AU)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(DK)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1DK)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(NO)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1NO)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(SE)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1SE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(IT)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1IT)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(FR)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1FR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(GB)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1GB)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(CA)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1CA)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(PL)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1PL)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(MY)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1MY)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(JP)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1JP)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(FI)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1FI)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(US)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1US)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(KR)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1KR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(PT)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1PT)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(IE)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1IE)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(BR)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1BR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(MX)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1MX)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(TR)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1TR)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(RU)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1RU)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(C1)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1C1)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(ZA)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1ZA)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(PX)                                                  
SET VALUE W460S3                                                                
  VCOM2(W460Z1PX)                                                               
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(IN)                                                  
 PASSIVATE W460J0Z1                                                             
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(CZ)                                                  
 PASSIVATE W460J0Z1                                                             
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(HU)                                                  
 PASSIVATE W460J0Z1                                                             
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(ES)                                                  
 PASSIVATE W460J0Z1                                                             
END-IF                                                                          
IF-SYMBOL W460S3 COUNTRYX2(AT)                                                  
 PASSIVATE W460J0Z1                                                             
END-IF                                                                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W460J305                                         
