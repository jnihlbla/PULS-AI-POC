//W426J011 JOB (650W4260100W426J011,W100),'RTN W426R2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM TIME=1,LINES=9,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P011                                                         
//SOP     EXEC WSOPEND,PROCESS=W426J011                                         
