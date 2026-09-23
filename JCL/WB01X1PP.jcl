//WB01X1PP JOB (640WB010100WB01X1PP,W100),'RTN WB01X1',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER WB01X1 SYMBOLS                                                          
    VCOM(WB01X1PP)                                                              
  END-ORDER                                                                     
//*                                                                             
