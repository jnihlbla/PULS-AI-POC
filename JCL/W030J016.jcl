//W030J016 JOB (640W0010300W030J016,W100),'RTN W030D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W030    EXEC W030P016                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W030J016                                         
