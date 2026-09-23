//W115JMSD   JOB (650W1150100W115JMSD,W100),'RTN W115D1',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//***********************************************************                   
//* ÖVERFÖRING AV FEL-FIL TILL DRIFT                                            
//***********************************************************                   
//MEMOSND EXEC WMEMOSND,DSIN=NULLFILE                                           
//APIFILE  DD *                                                                 
)SEND                                                                           
 TITLE FEL-MEDDELANDE                                                           
 OPTION FORCE                                                                   
 DEST GMAGNUSS(A)VOLVOCARS.COM                                                  
 DEST WSYST@VOLVOCARS.COM                       VCC11.WHELP                     
 MEMO MEMTXTD1                                                                  
)END                                                                            
/*                                                                              
//MEMTXTD1 DD DSN=W.QASE.CONSTANT(W115ME),DISP=SHR                              
//         DD DSN=W115.W115D1.W11521(+0),DISP=SHR                               
//SYSABEND DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//*                                                                             
//*RCTEST  EXEC VRCABEND,COND=(0,EQ,PDF.SDF080A)                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W115JMSD                                         
