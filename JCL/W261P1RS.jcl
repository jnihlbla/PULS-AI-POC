//W261P1RS JOB (650W2610100W261P1RS,W100),'RTN W261P1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ  LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//SOP     EXEC WSOPEND,PROCESS=W261P1RS                                         
/*                                                                              
