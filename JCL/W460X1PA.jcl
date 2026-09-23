//W460X1PA JOB (640W4600100W460X1PA,W100),'RTN W460E1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W460E1 SYMBOLS                                                          
    VCOM(W460X1PA)                                                              
  END-ORDER                                                                     
//*                                                                             
