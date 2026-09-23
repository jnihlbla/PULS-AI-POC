//W161X3MQ JOB (640W1610100W161X3MQ,W100),'RTN W161E3',                         
//             CLASS=K,USER=W0MQT01                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W161E3                                                                  
//*                                                                             
