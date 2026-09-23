//W510X1SE JOB (640W5100100W510X1SE,W100),'RTN W510X1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W510X1 SYMBOLS                                                          
    VCOM(W510X1SE)                                                              
  END-ORDER                                                                     
//*                                                                             
