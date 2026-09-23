//W461Z1ES JOB (670W4610100W461Z1ES,W100),'RTN W461D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* NOAC ÖVERFÖRING SPANIEN                                                     
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W461Z1ES                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W461.W461D2.W46199(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461Z1ES                                         
