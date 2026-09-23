//W114X3SE JOB (640W1140100W114X3SE,W100),'RTN W114X3',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W114X3 SYMBOLS                                                          
    VCOM(W114X3SE)                                                              
  END-ORDER                                                                     
//*                                                                             
