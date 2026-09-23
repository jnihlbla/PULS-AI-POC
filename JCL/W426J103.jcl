//W426J103 JOB (670W4260100W426J103,W100),'RTN W426R1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM TIME=1,LINES=9,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P103                                                         
//SOP     EXEC WSOPEND,PROCESS=W426J103                                         
