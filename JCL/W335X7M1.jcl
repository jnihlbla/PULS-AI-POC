//W335X7M1 JOB (640W3350100W335X7M1,W100),'RTN W335X7',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP2                                                            
//IN      DD   *                                                                
  ORDER W335X7 SYMBOLS                                                          
    VCOM(W335X7M1)                                                              
  END-ORDER                                                                     
  ORDER W335B2 SYMBOLS                                                          
     VCOM(W335Z1M1)                                                             
     PGM(W33541)                                                                
  END-ORDER                                                                     
//*                                                                             
//ABE     IF ABEND THEN                                                         
//SOP     EXEC WSOP,COMMAND='ABEND W335X7'                                      
//ABE     ENDIF                                                                 
