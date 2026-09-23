//W351X3FI JOB (640W3510100W351X3FI,W100),'RTN W351X3',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W351X3 SYMBOLS                                                          
    VCOM(W351X3FI)                                                              
  END-ORDER                                                                     
//*                                                                             
