//W114Z2SE JOB (640W1140100W114Z2SE,W100),'RTN W114D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=WUT.W114D5.W11452(+0)                               
//VCOM     EXEC W016P022,VCOM=W114Z2SE,COND=(0,LT,TOM.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114D5.W11452(+0),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114Z2SE                                         
