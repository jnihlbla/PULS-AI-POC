//W561J063 JOB (640W5100100W561J063,W100),'RTN W561D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W561    EXEC W561P063                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561J063                                         
