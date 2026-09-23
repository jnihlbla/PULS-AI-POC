//W432J010 JOB (650W4320100W432J010,W100),'RTN W400D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W432    EXEC W432P010                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W432J010                                         
