//W510J253 JOB (650W5100100W510J253,W100),'RTN W500V1',                         
//             USER=?,PASSWORD=?,CLASS=K                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W510    EXEC W510P253                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510J253                                         
