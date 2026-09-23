//W335X9M3 JOB (640W3350100W335X9M3,W100),'RTN W335X9',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP2                                                            
//IN      DD   *                                                                
  ORDER W335X9 SYMBOLS                                                          
    VCOM(W335X9M3)                                                              
  END-ORDER                                                                     
//*                                                                             
//ABE     IF ABEND THEN                                                         
//SOP     EXEC WSOP,COMMAND='ABEND W335X9'                                      
//ABE     ENDIF                                                                 
