//W231J077 JOB (650W2310100W231J077,W100),'RTN W231D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W231    EXEC W231P077                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W231J077                                         
