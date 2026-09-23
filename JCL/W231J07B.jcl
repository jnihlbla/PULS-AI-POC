//W231J07B JOB (650W2310100W231J07B,W100),'RTN W231S2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W231    EXEC W231P07B                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W231J07B                                         
