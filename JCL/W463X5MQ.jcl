//W463X5MQ JOB (640W4630100W463X5MQ,W100),'RTN W463E5',                         
//             CLASS=K,USER=W0MQT01                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP   EXEC WSOP2                                                              
//IN    DD *                                                                    
  ORDER W463E5                                                                  
//*                                                                             
//ABE     IF ABEND THEN                                                         
//SOP     EXEC WSOP,COMMAND='ABEND W463E5'                                      
//ABE     ENDIF                                                                 
