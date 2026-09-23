//WF10X1IN JOB (640WF100100WF10X1IN,W100),'RTN WF10X3',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER WF10X3 SYMBOLS                                                          
    VCOM(WF10X1IN)                                                              
  END-ORDER                                                                     
//*                                                                             
                                                                                
