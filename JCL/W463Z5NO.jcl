//W463Z5NO JOB (670W4630100W463Z5NO,W100),'RTN W463D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W463.W463D6.W4636L(+0)                              
//*                                                                             
//     IF (EMPTYT.T.RC = 0) THEN                                                
//* DIR BUSINESS, BILLINGTRANSAR TILL VIPS-NO                                   
//VCOM     EXEC W016P022,VCOM=W463Z5NO                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W463.W463D6.W4636L(+0),DISP=SHR                        
//*                                                                             
//     ENDIF                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463Z5NO                                         
