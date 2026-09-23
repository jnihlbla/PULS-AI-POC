//W272J0G1 JOB (640W2720100W272J0G1,W100),'RTN W271D8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W272    EXEC W272P0G1                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J0G1                                         
