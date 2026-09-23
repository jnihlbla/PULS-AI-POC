//W512J136 JOB (650W5120100W512J136,W100),'RTN W500M2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W512    EXEC W512P036,                                                        
//        INDUT=W512.W500M2                                                     
//W51236.W51236D2 DD DSN=W512.W512X1SE.W51211,DISP=SHR                          
//SOP     EXEC WSOPEND,PROCESS=W512J136                                         
