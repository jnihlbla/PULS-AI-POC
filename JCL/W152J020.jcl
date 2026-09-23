//W152J020 JOB (670W1520100W152J020,W100),'RTN W152V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W152    EXEC W152P020                                                         
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W152J020                                         
