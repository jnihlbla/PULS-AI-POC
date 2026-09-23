//W560ZBUS JOB (670W5600100W560ZBUS,W100),'RTN W560R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING TILL USA                                                    
//***  SALES AND TRANSACTION LIST TILL ATLANTA                                  
//VCOM     EXEC W016P022,VCOM=W560ZBUS                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W560.DC44.W56050(+0),DISP=SHR                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560ZBUS                                         
