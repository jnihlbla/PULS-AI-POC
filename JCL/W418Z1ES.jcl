//W418Z1ES JOB (670W4180100W418Z1ES,W100),'RTN W418D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING SPANIEN                                                     
//*************  VCAS                                                           
//TOM      EXEC WEMPTST,DSIN=W418.W418D2.W4183Q(+0)                             
//VCOM     EXEC W016P022,VCOM=W418Z1ES,COND=(0,LT,TOM.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W418.W418D2.W4183Q(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418Z1ES                                         
