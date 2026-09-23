//W512J132 JOB (650W5120100W512J032,W100),'RTN W500M2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W512    EXEC W512P032,                                                        
//        INDUT=W512.W500M2                                                     
//SOP     EXEC WSOPEND,PROCESS=W512J132                                         
