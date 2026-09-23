//W570X1CN JOB (640W5700100W570X1CN,W100),'RTN W570X1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W570X1 SYMBOLS                                                          
    VCOM(W570X1CN)                                                              
  END-ORDER                                                                     
//*                                                                             
