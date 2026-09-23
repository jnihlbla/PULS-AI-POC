//W461Z1IN JOB (640W4610100W461Z1IN,W100),'RTN W461D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=W461.W461D4.W461IN2(+0)                             
//VCOM    EXEC W016P022,VCOM=W461Z1IN,COND=(0,LT,TOM.T)                         
//*                                                                             
//W01622.W016ZZD1 DD DSN=W461.W461D4.W461IN2(+0),DISP=SHR                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461Z1IN                                         
