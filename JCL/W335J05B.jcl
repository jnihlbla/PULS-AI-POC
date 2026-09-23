//W335J05B JOB (670W3350100W335J05B,W100),'RTN W335D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT2 EXEC WEMPTST,DSIN=W335.W335D5.W33551(+0)                              
//*                                                                             
//    IF (EMPTYT2.T.RC = 0) THEN                                                
//*                                                                             
//* ERSÄTTNINGAR VECKOBASIS                                                     
//*************  VCNA                                                           
//VCOM     EXEC W016P022,VCOM=W335Z4M5                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335D5.W33551(+0),DISP=SHR                        
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J05B                                         
