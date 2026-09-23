//W560ZCUS JOB (670W5600100W560ZCUS,W100),'RTN W560D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING TILL USA                                                    
//***  STOCK LIST NDC NA                                                        
//VCOM     EXEC W016P022,VCOM=W560ZCUS                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W560.W560D1.W56056(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560ZCUS                                         
