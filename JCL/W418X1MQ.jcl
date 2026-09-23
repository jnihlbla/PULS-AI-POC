//W418X1MQ JOB (640W4180100W418X1MQ,W100),'RTN W418E4',                         
//             CLASS=K,USER=W0MQT01                                             
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W418E4                                                                  
//*                                                                             
