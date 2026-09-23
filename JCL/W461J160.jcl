//W461J160 JOB (650W4610100W461J160,W100),'RTN W461V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//W461    EXEC W461P160                                                         
//*                                                                             
//  IF (W461.W46160.RC = 2) THEN                                                
//MEMO    EXEC WMEMOSND                                                         
//M.APIFILE  DD DSN=W.QASE.CONSTANT(W46160ME),DISP=SHR                          
//M.SEND     DD DUMMY                                                           
//  ENDIF                                                                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W461J160                                         
//*                                                                             
