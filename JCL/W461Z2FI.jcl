//W461Z2FI JOB (670W4610100W461Z2FI,W100),'RTN W461V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* NOAC ÖVERFÖRING FINLAND 1091                                                
//*************  VCAS                                                           
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W461Z1FI                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W461.W461V1.W46148(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461Z2FI                                         
