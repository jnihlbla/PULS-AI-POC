//W476Z4CH JOB (640W4760100W476Z4CH,W100),'RTN W476S9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* NOAC ÖVERFÖRING SCHWEIZ 2078                                                
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W476Z4CH                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W476.W476S9.W47656A(+0),DISP=SHR                       
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476Z4CH                                         
