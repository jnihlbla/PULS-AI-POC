//W335X8M1 JOB (640W3350100W335X8M1,W100),'RTN W335X8',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP2                                                            
//IN      DD   *                                                                
  ORDER W335X8 SYMBOLS                                                          
    VCOM(W335X8M1)                                                              
  END-ORDER                                                                     
  ORDER W335B3 SYMBOLS                                                          
     VCOM(W335Z3M1)                                                             
  END-ORDER                                                                     
//*                                                                             
//ABE     IF ABEND THEN                                                         
//SOP     EXEC WSOP,COMMAND='ABEND W335X8'                                      
//ABE     ENDIF                                                                 
