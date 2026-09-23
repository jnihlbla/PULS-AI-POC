//W418Z1C1 JOB (670W4180100W418Z1C1,W100),'RTN W418D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING KINA                                                        
//*************  VCCS                                                           
//TOM      EXEC WEMPTST,DSIN=W418.W418D2.W418C1(+0)                             
//VCOM     EXEC W016P022,VCOM=W418Z1C1,COND=(0,LT,TOM.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W418.W418D2.W418C1(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418Z1C1                                         
