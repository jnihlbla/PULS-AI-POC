//W461J0D6 JOB (650W4610100W461J0D6,W100),'RTN W461D2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*461    EXEC W461P0D6                                                         
//SOP     EXEC WSOPEND,PROCESS=W461J0D6                                         
