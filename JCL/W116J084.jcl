//W116J084 JOB (640W1160100W116J084,W100),'RTN W116D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W116    EXEC W116P084                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J084                                         
