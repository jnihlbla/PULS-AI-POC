//W476Z4JP JOB (670W4760100W476Z4JP,W100),'RTN W476D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* FAKTURAUPPG FÖR BROKERS I JAPAN                                             
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=WUT.W476D1.W476JP(+0)                               
//*                                                                             
//   IF (EMPTYT.T.RC = 4) THEN                                                  
//DEL1    EXEC PGM=IEFBR14                                                      
//DD      DD DSN=WUT.W476D1.W476JP(+0),DISP=(OLD,DELETE)                        
//   ELSE                                                                       
//VCOM    EXEC W016P022,VCOM=W476Z4JP                                           
//W01622.W016ZZD1 DD DSN=WUT.W476D1.W476JP(+0),DISP=SHR                         
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476Z4JP                                         
