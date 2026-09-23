//W432J030 JOB (650W2110100W211J102,W100),'RTN W432V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W432    EXEC W432P030                                                         
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W432J030                                         
