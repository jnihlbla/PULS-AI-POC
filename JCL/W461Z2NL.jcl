//W461Z2NL JOB (670W4610100W461Z2NL,W100),'RTN W461V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* NOAC ÖVERFÖRING HOLLAND                                                     
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W461Z1NL                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W461.W461V1.W46181(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461Z2NL                                         
