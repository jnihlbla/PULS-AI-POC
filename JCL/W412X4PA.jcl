//W412X1PA JOB (640W4120100W412X4PA,W100),'RTN W412E4',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W412E4 SYMBOLS                                                          
    VCOM(W412X4PA)                                                              
  END-ORDER                                                                     
//*                                                                             
