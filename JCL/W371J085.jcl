//W371J085 JOB (650W3710100W371J085,W100),'RTN W371S2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM LINES=999,FORMS=1800,LINECT=0                                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P085                                                         
//SOP     EXEC WSOPEND,PROCESS=W371J085                                         
