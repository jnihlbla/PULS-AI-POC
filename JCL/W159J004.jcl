//W159J004 JOB (670W1590100W159J004,W100),'RTN W159D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W159    EXEC W159P004                                                         
//*                                                                             
//W15904.SYSIN DD *                                                             
N                                                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W159J004                                         
