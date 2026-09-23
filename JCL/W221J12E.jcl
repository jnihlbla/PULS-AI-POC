//W221J12E JOB (640W2210100W221J12E,W100),'RTN W221D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W221    EXEC W221P12E                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J12E                                         
