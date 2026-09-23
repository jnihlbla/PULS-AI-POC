//W331J039 JOB (670W3310100W331J039,W100),'RTN W331V5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT2 EXEC WEMPTST,DSIN=W331.W331V5.W33139(+0)                              
//*                                                                             
//    IF (EMPTYT2.T.RC = 0) THEN                                                
//*                                                                             
//* ERSÄTTNINGAR VECKOBASIS                                                     
//*************  S&T                                                            
//VCOM     EXEC W016P022,VCOM=W331Z2P0                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W331.W331V5.W33139(+0),DISP=SHR                        
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W331J039                                         
