//W560ZAUS JOB (670W5600100W560ZAUS,W100),'RTN W560R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING TILL USA                                                    
//***  REPORT AVERAGE VS REPLACEMENT COST TILL ROCKLEY                          
//VCOM     EXEC W016P022,VCOM=W560ZAUS                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W560.W560R1.W56082(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560ZAUS                                         
