//W461J009 JOB (650W4610100W461J009,W100),'RTN W461D1',                         
//             USER=?,PASSWORD=?,                                               
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P009                                                         
//SOP     EXEC WSOPEND,PROCESS=W461J009                                         
