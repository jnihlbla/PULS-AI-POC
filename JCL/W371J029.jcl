//W371J029 JOB (650W3710100W371J029,W100),'RTN W371V5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
/*JOBPARM LINES=999                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P029                                                         
//SOP     EXEC WSOPEND,PROCESS=W371J029                                         
