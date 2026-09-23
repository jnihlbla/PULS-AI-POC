//W461Z1IT JOB (670W4610100W461Z1IT,W100),'RTN W461D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* NOAC ÖVERFÖRING ITALIEN                                                     
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W461Z1IT                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W461.W461D2.W46147(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461Z1IT                                         
