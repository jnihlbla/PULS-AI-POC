//W116Z2C1 JOB (640W1160100W116Z2C1,W100),'RTN W116D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=W116.W116D4.W1167B(+0)                              
//VCOM    EXEC W016P022,VCOM=W116Z2C1,COND=(0,LT,TOM.T)                         
//*                                                                             
//W01622.W016ZZD1 DD DSN=W116.W116D4.W1167B(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116Z2C1                                         
