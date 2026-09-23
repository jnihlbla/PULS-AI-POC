//W561X1US JOB (640W5100100W561X1US,W100),'RTN W561X1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W561X1 SYMBOLS                                                          
    VCOM(W561X1US)                                                              
  END-ORDER                                                                     
//*                                                                             
