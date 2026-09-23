//WF10X1US JOB (640WF100100WF10X1US,W100),'RTN WF10X4',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER WF10X4 SYMBOLS                                                          
    VCOM(WF10X1US)                                                              
  END-ORDER                                                                     
//*                                                                             
                                                                                
