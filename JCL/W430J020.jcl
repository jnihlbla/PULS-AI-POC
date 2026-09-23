//W430J020 JOB (650W4300100W430J020,W100),'RTN W430Y1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=M                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W430    EXEC W430P020                                                         
//SOP     EXEC WSOPEND,PROCESS=W430J020                                         
