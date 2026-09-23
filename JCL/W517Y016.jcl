//W517Y016 JOB (650W5170100W517Y016,W100),'RTN WYR001',                         
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
//       EXEC W517P016,INDUT=W517.WYR001                                        
//W51716.W51716D1 DD DSN=W011.W01172(+0),DISP=SHR                               
//SOP     EXEC WSOPEND,PROCESS=W517Y016                                         
