//W463X7SE JOB (640W4630100W463X7SE,W100),'RTN W463X7',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP2                                                            
//IN      DD *                                                                  
  ORDER W463X7 SYMBOLS                                                          
    VCOM(W463X7SE)                                                              
  END-ORDER                                                                     
//*                                                                             
//ABE     IF ABEND THEN                                                         
//SOP     EXEC WSOP,COMMAND='ABEND W463X7'                                      
//ABE     ENDIF                                                                 
