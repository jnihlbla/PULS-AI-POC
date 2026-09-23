//W213J023 JOB (640W2130100W213J023,W100),'RTN W213P1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W213    EXEC W213P023                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W213J023                                         
