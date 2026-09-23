//W461Z3JP JOB (670W4610100W461Z3JP,W100),'RTN W461D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* NOAC ÖVERFÖRING JAPAN 5222                                                  
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W461Z1JP                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W461.W461D4.W4612S(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461Z3JP                                         
