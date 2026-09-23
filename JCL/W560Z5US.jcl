//W560Z5US JOB (670W5600100W560Z5US,W100),'RTN W560Y3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING TILL USA                                                    
//***  AGING REPORT TO ATLANTA                                                  
//VCOM     EXEC W016P022,VCOM=W560Z5US                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W560.W560Y3.W56085(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560Z5US                                         
