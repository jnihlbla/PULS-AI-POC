//W213X1SE JOB (640W2130100W213X1SE,W100),'RTN W213X1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W213X1 SYMBOLS                                                          
    VCOM(W213X1SE)                                                              
  END-ORDER                                                                     
//*                                                                             
