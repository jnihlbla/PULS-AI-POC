//W560Z1US JOB (670W5600100W560Z1US,W100),'RTN W560D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING USA                                                         
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W560Z1US                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W560.W560D1.W56022(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560Z1US                                         
