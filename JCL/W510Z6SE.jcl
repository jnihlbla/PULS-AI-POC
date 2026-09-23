//W510Z6SE JOB (670W5100100W510Z6SE,W100),'RTN W510D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* TILL LEVA POSTTYP 504 505                                                   
//*************  VCCS TORSLANDAVERKEN                                           
//EMPTY1  EXEC WEMPTST,DSIN=WUT.W510D2.W5106E(+0)                               
//   IF (EMPTY1.T.RC = 0) THEN                                                  
//VCOM     EXEC W016P022,VCOM=W510Z6SE                                          
//W01622.W016ZZD1 DD DSN=WUT.W510D2.W5106E(+0),DISP=SHR                         
//   ENDIF                                                                      
//*                                                                             
//* TILL LEVA POSTTYP 504 505                                                   
//*************  VCCS KOMPONENTER                                               
//EMPTY2  EXEC WEMPTST,DSIN=WUT.W510D2.W5106H(+0)                               
//   IF (EMPTY2.T.RC = 0) THEN                                                  
//VCOM     EXEC W016P022,VCOM=W510Z6SE                                          
//W01622.W016ZZD1 DD DSN=WUT.W510D2.W5106H(+0),DISP=SHR                         
//   ENDIF                                                                      
//*                                                                             
//* TILL LEVA POSTTYP 504 505                                                   
//*************  VCCS MAASTRISCHT                                               
//EMPTY3  EXEC WEMPTST,DSIN=WUT.W510D2.W51065(+0)                               
//   IF (EMPTY3.T.RC = 0) THEN                                                  
//VCOM     EXEC W016P022,VCOM=W510Z6SE                                          
//W01622.W016ZZD1 DD DSN=WUT.W510D2.W51065(+0),DISP=SHR                         
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510Z6SE                                         
