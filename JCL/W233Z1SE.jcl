//W233Z1SE JOB (640W2330100W233Z1SE,W100),'RTN W233PV',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*        THE W233Z1SE JCL IS USING THE W233Z7SE PARTNER                       
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=W233.W233PV.W23314(+0)                              
//VCOM    EXEC W016P022,VCOM=W233Z7SE,COND=(0,LT,TOM.T)                         
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23314(+0),DISP=SHR                        
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=W233.W233PV.W23315(+0)                              
//VCOM    EXEC W016P022,VCOM=W233Z7SE,COND=(0,LT,TOM.T)                         
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23315(+0),DISP=SHR                        
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W233Z1SE                                         
