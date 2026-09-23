//W970B3RS JOB (640W0000100W970B3RS,W100),'RTN W970B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*                                                                             
//        EXEC WSOP                                                             
SET VALUE W970B3                                                                
 ACLGRP(&ACLGRP)                                                                
END-SET                                                                         
IF-SYMBOL W970B3 ACLGRP(ENDE)                                                   
  ORDER W970J032                                                                
END-IF                                                                          
IF-SYMBOL W970B3 ACLGRP(ADB2)                                                   
  ORDER W970J033 SYMBOLS                                                        
    DB2ENV(ACPT) DB2SYS(D2F1)                                                   
  END-ORDER                                                                     
END-IF                                                                          
IF-SYMBOL W970B3 ACLGRP(QDB2)                                                   
  ORDER W970J033 SYMBOLS                                                        
    DB2ENV(QASE) DB2SYS(D2G1)                                                   
  END-ORDER                                                                     
END-IF                                                                          
IF-SYMBOL W970B3 ACLGRP(PMUN)                                                   
  ORDER W970J034 SYMBOLS                                                        
    UXGRP(PMUX) RULES(PMUR) BIND(SD01) ROUTE(NJESD)                             
  END-ORDER                                                                     
END-IF                                                                          
IF-SYMBOL W970B3 ACLGRP(AMUN)                                                   
  ORDER W970J034 SYMBOLS                                                        
    UXGRP(AMUX) RULES(AMUR) BIND(SD01) ROUTE(NJESD)                             
  END-ORDER                                                                     
END-IF                                                                          
IF-SYMBOL W970B3 ACLGRP(QRUN)                                                   
  ORDER W970J034 SYMBOLS                                                        
    UXGRP(QRUX) RULES(QRUR) BIND(VCC1) ROUTE(NJEVC)                             
  END-ORDER                                                                     
END-IF                                                                          
IF-SYMBOL W970B3 ACLGRP(ARUN)                                                   
  ORDER W970J034 SYMBOLS                                                        
    UXGRP(ARUX) RULES(ARUR) BIND(VCC2) ROUTE(NJEVC)                             
  END-ORDER                                                                     
END-IF                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970B3RS                                         
