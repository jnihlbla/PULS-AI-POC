//W221Z3SE JOB (640W2210100W221Z3SE,W100),'RTN W221D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* VCOM ÖVERFÖRING TMS CALLOFF INBOUND                                         
//*                                                                             
//TOM      EXEC WEMPTST,DSIN=W221.W221D2.W22133(+0)                             
//VCOM     EXEC W016P022,VCOM=W221Z3SE,COND=(0,LT,TOM.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W221.W221D2.W22133(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221Z3SE                                         
