//W271J0G1 JOB (640W2710100W271J0G1,W100),'RTN W271D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W271    EXEC W271P0G1                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J0G1                                         
