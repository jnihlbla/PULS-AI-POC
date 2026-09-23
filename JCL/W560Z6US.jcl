//W560Z6US JOB (670W5600100W560Z6US,W100),'RTN W560Y1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING TILL USA                                                    
//***  THE END YEAR REPORT FOR USA  (THE OLD VERSION)                           
//VCOM     EXEC W016P022,VCOM=W560Z6US                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W560.W560Y1.W56066(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560Z6US                                         
