//W224S2RS JOB (640W2240100W224S2RS,W100),'RTN W224S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*  PART2 = &LT                                                                
//*                                                                             
//SOPSYMB  EXEC WSOP                                                            
SET VALUE W224S2                                                                
  PART2(&LT)                                                                    
END-SET                                                                         
IF-SYMBOL W224S2 PART2(++)                                                      
  CANCEL W224J074                                                               
  CANCEL W224J072                                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224S2RS                                         
