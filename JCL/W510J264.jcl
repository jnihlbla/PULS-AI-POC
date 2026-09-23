//W510J264 JOB (670W5100100W510J264,W100),'RTN WYR001',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W510    EXEC W510P264                                                         
//W51064.W51064D3 DD  DUMMY                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510J264                                         
