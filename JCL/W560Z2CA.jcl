//W560Z2CA JOB (650W5600100W560Z2CA,W100),'RTN W560R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING TILL CANADA                                                 
//***  SALES AND TRANSACTION LIST TILL TORONTO                                  
//*************  PULS                                                           
//VCOM     EXEC W016P022,VCOM=W560Z2CA                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W560.DC51.W56050(+0),DISP=SHR                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560Z2CA                                         
