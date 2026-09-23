//W092X4MQ JOB (640W0920100W092X4MQ,W100),'RTN W092E4',                         
//             CLASS=K,USER=W0MQT01                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W092E4                                                                  
//*                                                                             
