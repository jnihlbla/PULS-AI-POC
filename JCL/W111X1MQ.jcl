//W111X1MQ JOB (640W1110100W111X1MQ,W100),'RTN W111E1',                         
//             CLASS=K,USER=W0MQT01                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W111E1                                                                  
//*                                                                             
