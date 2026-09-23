//W476Z3NL JOB (640W4760100W476Z3NL,W100),'RTN W476SH',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* VCOM     - FIL TILL HOLLAND TULL                                            
//*                                                                             
//VCOM    EXEC W016P022,VCOM=W476Z3NL                                           
//W01622.W016ZZD1 DD DSN=W476.W476SH.W47686(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476Z3NL                                         
