//WF10X1SE JOB (640WF100100WF10X1SE,W100),'RTN WF10X2',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER WF10X2 SYMBOLS                                                          
    VCOM(WF10X1SE)                                                              
  END-ORDER                                                                     
//*                                                                             
                                                                                
