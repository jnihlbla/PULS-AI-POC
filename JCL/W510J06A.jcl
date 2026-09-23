//W510J06A JOB (670W5100100W510J06A,W100),'RTN W510S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//* DELAY THE EXECUTION TO AVOID CONFLICT WITH WF21J006                         
//WAIT    EXEC WWAIT,SECONDS=30                                                 
//W510    EXEC W510P06A                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510J06A                                         
