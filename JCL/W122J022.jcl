//W122J022 JOB (670W1220100W122J022,W100),'RTN W122B2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE    XEQ LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W122    EXEC W122P022                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W122J022                                         
