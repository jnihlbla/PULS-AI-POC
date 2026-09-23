//W111X1SE JOB (640W1110100W111X1SE,W100),'RTN W111X1',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W111X1 SYMBOLS                                                          
    VCOM(W111X1SE)                                                              
  END-ORDER                                                                     
//*                                                                             
