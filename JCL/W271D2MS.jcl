//W092D2MS   JOB (650W0920100W092D2MS,W100),'RTN W092D2',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//***********************************************************                   
//* INFO  ARTIKLAR MED FEL VID LEVERANTÖRNRKONVERTERING                         
//* CARPAK                                                                      
//***********************************************************                   
//MEMOSND EXEC WMEMOSND,DSIN=NULLFILE                                           
//APIFILE  DD *                                                                 
)SEND                                                                           
TITLE FEL LEVNR NL                                                              
OPTION FORCE                                                                    
DEST MONICA.BERGSTROM(A)VOLVO.COM                                               
 MEMO MEMTXTD1                                                                  
)END                                                                            
/*                                                                              
//MEMTXTD1 DD DSN=W092.FEL.INKOEP(+0),DISP=SHR                                  
//SYSABEND DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//*                                                                             
//*RCTEST  EXEC VRCABEND,COND=(0,EQ,PDF.SDF080A)                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W092D2MS                                         
