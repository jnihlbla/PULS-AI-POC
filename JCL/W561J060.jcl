//W561J060 JOB (670W5100100W561J060,W100),'RTN W561D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//      INCLUDE MEMBER=SYST4                                                    
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W561    EXEC W561P060                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561J060                                         
