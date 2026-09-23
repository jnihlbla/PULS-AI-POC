//W335J199 JOB (640W3350100W335J199,W100),'RTN W335V6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* S6T COSTS/DAY/MONTH                                                         
//************* TO CENTRAL S& T SYSTEM                                          
//VCOM     EXEC W016P022,VCOM=W335Z7B                                           
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335V6.W3359A(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J199                                         
