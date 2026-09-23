//W371J061 JOB (670W3710100W371J061,W100),'RTN W371V4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WAIT    EXEC WWAIT,SECONDS=200                                                
//W371    EXEC W371P061                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J061                                         
