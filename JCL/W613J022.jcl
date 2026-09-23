//W613J022 JOB (640W6130100W613J022,W100),'RTN W613V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W613    EXEC W613P022                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W613J022                                         
