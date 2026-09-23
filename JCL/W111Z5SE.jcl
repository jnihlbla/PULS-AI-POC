//W111Z5SE JOB (640W1110100W111Z5SE,W100),'RTN W111V5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=W111.W111V5.W1117A(+0)                              
//VCOM    EXEC W016P022,VCOM=W111Z5SE,COND=(0,LT,TOM.T)                         
//*                                                                             
//W01622.W016ZZD1 DD DSN=W111.W111V5.W1117A(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111Z5SE                                         
