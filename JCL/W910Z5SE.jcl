//W910Z5SE JOB (640W9100100W910Z5SE,W100),'RTN W910V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=W910.W910V2.W9106A(+0)                              
//VCOM    EXEC W016P022,VCOM=W111Z5SE,COND=(0,LT,TOM.T)                         
//*                                                                             
//W01622.W016ZZD1 DD DSN=W910.W910V2.W9106A(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910Z5SE                                         
