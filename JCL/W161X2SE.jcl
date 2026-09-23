//W161X2SE JOB (640W1610100W161X2SE,W100),'RTN W161X2',                         
//             CLASS=K                                                          
/*JOBPARM TIME=1,LINES=5,FORMS=1800,LINECT=0                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W161X2 SYMBOLS                                                          
    VCOM(W161X2SE)                                                              
  END-ORDER                                                                     
//*                                                                             
