//W030J100 JOB (650W0010300W030J100,W100),'RTN W030D2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W030    EXEC W030P100                                                         
//SOP     EXEC WSOPEND,PROCESS=W030J100                                         
