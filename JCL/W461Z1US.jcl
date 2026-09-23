//W461Z1US JOB (640W4610100W461Z1US,W100),'RTN W461D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* NOAC ÖVERFÖRING USA                                                         
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W461Z1US                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W461.W461D3.W4612P(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461Z1US                                         
