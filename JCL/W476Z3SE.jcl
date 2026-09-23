//W476Z3SE JOB (670W4760100W476Z3SE,W100),'RTN W476SA',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*                                                                             
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* FAKTURAUPPG FÖR ÖVERFÖRING TILL TLS                                         
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W476.W476SA.W47681(+0)                              
//*                                                                             
//   IF (EMPTYT.T.RC = 4) THEN                                                  
//DEL   EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W476.W476SA.W47681(+0),DISP=(OLD,DELETE)                         
//*  ELSE                                                                       
//*VCOM     EXEC W016P022,VCOM=W476Z3SE                                         
//*                                                                             
//*W01622.W016ZZD1 DD DSN=W476.W476SA.W47681(+0),DISP=SHR                       
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W476.W476SA.W47681X(+0)                             
//*                                                                             
//   IF (EMPTYT1.T.RC = 4) THEN                                                 
//DEL1  EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W476.W476SA.W47681X(+0),DISP=(OLD,DELETE)                        
//DEL2  EXEC PGM=IEFBR14                                                        
//DD2   DD DSN=W476.W476SA.W47681XC(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//MQSEN   EXEC WZ11P022,ABSADDRS=CARPARTS.FLS.EXPORTSHIPMENT,                   
//             DSIN=W476.W476SA.W47681X(+0)                                     
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476Z3SE                                         
