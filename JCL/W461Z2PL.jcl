//W461Z2PL JOB (670W4610100W461Z2PL,W100),'RTN W461V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* NOAC ÖVERFÖRING POLEN                                                       
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W461Z1PL                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W461.W461V1.W4614C(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461Z2PL                                         
