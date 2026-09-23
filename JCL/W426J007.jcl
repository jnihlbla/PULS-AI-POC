//W426J007 JOB (650W4260100W426J007,W100),'RTN W426D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM TIME=1,LINES=99,CARDS=0,FORMS=1800                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P007                                                         
//SOP     EXEC WSOPEND,PROCESS=W426J007                                         
