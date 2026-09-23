//W426J104 JOB (670W4260100W426J104,W100),'RTN W426D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM TIME=1,LINES=9,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P104                                                         
//SOP     EXEC WSOPEND,PROCESS=W426J104                                         
