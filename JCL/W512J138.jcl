//W512J138 JOB (650W5120100W512J138,W100),'RTN W500M2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W512    EXEC W512P038,                                                        
//        INDIN=W512.W500M2,INDUT=W512.W500M2                                   
//SOP     EXEC WSOPEND,PROCESS=W512J138                                         
