//W371J004 JOB (670W3710100W371J004,W100),'RTN W371S6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* IDDC    = &IDDC                                                             
//* IDFAKT  = &IDFAKT                                                           
//* IDKOLLI = &IDKOLLI                                                          
//*                                                                             
//W371    EXEC W371P004                                                         
//W37104.SYSIN    DD *                                                          
&IDDC &IDFAKT &IDKOLLI                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J004                                         
