//W116Z6SE JOB (640WZ110100W116Z6SE,W100),'RTN W116D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//**********************************************************                    
//*   SEND FILE TO VSIM                                                         
//**********************************************************                    
//TOM1    EXEC WEMPTST,DSIN=W116.W116D7.W1166F(+0)                              
//WQSEN1  EXEC WZ11P022,COND=(0,LT,TOM1.T),                                     
//             ABSADDRS=CARPARTS.VSIM.PARTSINFO,                                
//             DSIN=W116.W116D7.W1166F(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116Z6SE                                         
