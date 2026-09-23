//W271J811 JOB (670W2710100W271J811,W100),'RTN W271D8',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P811,                                                        
//             INDIN=W271.W271D8                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J811                                         
