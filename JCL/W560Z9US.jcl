//W560Z9US JOB (670W5600100W560Z9US,W100),'RTN W560Y4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING TILL USA                                                    
//***  THE LIFO IN TRANSIT LISTS FOR USA (THE NEW VERSION)                      
//VCOM     EXEC W016P022,VCOM=W560Z9US                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W560.W560Y4.W56077(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560Z9US                                         
