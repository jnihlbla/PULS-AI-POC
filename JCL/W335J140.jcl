//W335J140 JOB (670W3350100W335J140,W100),'RTN W335B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*ANVÄNDER SAMMA PROCEDUR SOM J040 I RUTIN W335D5                              
//W335     EXEC W335P140,                                                       
//             INDIN2=W335.W335B2,                                              
//             INDUT=W335.W335B2                                                
//*                                                                             
//SOPSET     EXEC    WSOP                                                       
SET VALUE W335B2                                                                
 MARKET(&MARKET)                                                                
END-SET                                                                         
IF-SYMBOL W335B2 MARKET(M0)                                                     
SET VALUE W335B2                                                                
  MKOMP(MA)                                                                     
  VCOM(W335Z1M0)                                                                
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W335B2 MARKET(M2)                                                     
SET VALUE W335B2                                                                
  MKOMP(MC)                                                                     
  VCOM(W335Z1M2)                                                                
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W335B2 MARKET(M3)                                                     
SET VALUE W335B2                                                                
  MKOMP(MD)                                                                     
  VCOM(W335Z1M3)                                                                
END-SET                                                                         
END-IF                                                                          
IF-SYMBOL W335B2 MARKET(M5)                                                     
SET VALUE W335B2                                                                
  MKOMP(MF)                                                                     
  VCOM(W335Z1M5)                                                                
END-SET                                                                         
END-IF                                                                          
//*                                                                             
//SOPBEST EXEC WSOP                                                             
IF-SYMBOL W335B2 MARKET(M0)                                                     
   ORDER W335J14A SYMBOLS                                                       
     MKOMP(MA)                                                                  
   END-ORDER                                                                    
   ORDER W335J141 SYMBOLS                                                       
     VCOM(W335Z1M0)                                                             
   END-ORDER                                                                    
END-IF                                                                          
IF-SYMBOL W335B2 MARKET(M2)                                                     
   ORDER W335J14A SYMBOLS                                                       
     MKOMP(MC)                                                                  
   END-ORDER                                                                    
   ORDER W335J141 SYMBOLS                                                       
     VCOM(W335Z1M2)                                                             
   END-ORDER                                                                    
END-IF                                                                          
IF-SYMBOL W335B2 MARKET(M3)                                                     
   ORDER W335J14A SYMBOLS                                                       
     MKOMP(MD)                                                                  
   END-ORDER                                                                    
   ORDER W335J141 SYMBOLS                                                       
     VCOM(W335Z1M3)                                                             
   END-ORDER                                                                    
END-IF                                                                          
IF-SYMBOL W335B2 MARKET(M5)                                                     
 ORDER W335J144                                                                 
 ORDER W335J142 SYMBOLS                                                         
   VCOM(W335Z1M5)                                                               
 END-ORDER                                                                      
END-IF                                                                          
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J140                                         
