//W335J026 JOB (670W3350100W335J026,W100),'RTN W335V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT2 EXEC WEMPTST,DSIN=W335.W335V3.W33525(+0)                              
//*                                                                             
//    IF (EMPTYT2.T.RC = 0) THEN                                                
//*                                                                             
//* KAMPANJINFO VECKOBASIS                                                      
//*************                                                                 
//VCOM     EXEC W016P022,VCOM=W335Z8M2                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335V3.W33525(+0),DISP=SHR                        
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J026                                         
