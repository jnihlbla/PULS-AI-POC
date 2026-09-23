//W114Z1SE JOB (640W1140100W114Z1SE,W100),'RTN W114S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=WUT.W114S2.W11450(+0)                               
//VCOM     EXEC W016P022,VCOM=W114Z1SE,COND=(0,LT,TOM.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W114S2.W11450(+0),DISP=SHR                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114Z1SE                                         
