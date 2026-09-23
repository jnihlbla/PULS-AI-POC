//W271J0G2 JOB (670W2710100W271J0G2,W100),'RTN W271D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W271    EXEC W271P0G2                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J0G2                                         
