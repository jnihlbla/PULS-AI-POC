//W412X4PP JOB (640W4120100W412X4PP,W100),'RTN W412X4',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W412X4 SYMBOLS                                                          
    VCOM(W412X4PP)                                                              
  END-ORDER                                                                     
//*                                                                             
