//WB11J022 JOB (640WB010100WB11J022,W100),'RTN WB01D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTB                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WB11    EXEC WB11P022                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WB11J022                                         
