//W510J074 JOB (650W5100100W510J074,W100),'RTN W510D4',                         
//             USER=?,PASSWORD=?,CLASS=K                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W510    EXEC W510P074                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510J074                                         
