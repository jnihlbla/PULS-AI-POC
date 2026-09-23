//W272J0G2 JOB (670W2720100W272J0G2,W100),'RTN W271D8',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W272    EXEC W272P0G2                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J0G2                                         
