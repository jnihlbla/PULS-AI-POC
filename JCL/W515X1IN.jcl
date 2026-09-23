//W515X1IN JOB (640W5100100W515X1IN,W100),'RTN W515X1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W515X1 SYMBOLS                                                          
    VCOM(W515X1IN)                                                              
  END-ORDER                                                                     
//*                                                                             
