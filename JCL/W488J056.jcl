//W488J056 JOB (650W4880100W488J056,W100),                                      
//             'RTN W488D1',CLASS=K,                                            
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM LINES=999,FORMS=1800,LINECT=0                                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W488    EXEC W488P056                                                         
//SOP     EXEC WSOPEND,PROCESS=W488J056                                         
