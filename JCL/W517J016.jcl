//W517J016 JOB (650W5170100W517J016,W100),'RTN W517V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST6                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//       EXEC W517P016                                                          
//SOP     EXEC WSOPEND,PROCESS=W517J016                                         
