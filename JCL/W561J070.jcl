//W561J070 JOB (640W5100100W561J070,W100),'RTN W561D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W561    EXEC W561P070                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561J070                                         
