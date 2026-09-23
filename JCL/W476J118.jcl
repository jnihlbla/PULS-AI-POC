//W476J118 JOB (670W4760100W476J118,W100),'RTN W476S7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W476P118                                                         
//*************TAR BORT TOMMA FILER                                             
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W476.W476S7.W476IT7(+1)                             
//*                                                                             
//   IF (EMPTYT1.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W476.W476S7.W476IT7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//*                                                                             
//EMPTYT2 EXEC WEMPTST,DSIN=W476.W476S7.W476CH7(+1)                             
//*                                                                             
//   IF (EMPTYT2.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD2   DD DSN=W476.W476S7.W476CH7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT3 EXEC WEMPTST,DSIN=W476.W476S7.W476US7(+1)                             
//*                                                                             
//   IF (EMPTYT3.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD3   DD DSN=W476.W476S7.W476US7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT4 EXEC WEMPTST,DSIN=W476.W476S7.W476ES7(+1)                             
//*                                                                             
//   IF (EMPTYT4.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD4   DD DSN=W476.W476S7.W476ES7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT5 EXEC WEMPTST,DSIN=W476.W476S7.W476AT7(+1)                             
//*                                                                             
//   IF (EMPTYT5.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD5   DD DSN=W476.W476S7.W476AT7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT6 EXEC WEMPTST,DSIN=W476.W476S7.W476FR7(+1)                             
//*                                                                             
//   IF (EMPTYT6.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD6   DD DSN=W476.W476S7.W476FR7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT7 EXEC WEMPTST,DSIN=W476.W476S7.W476SE7(+1)                             
//*                                                                             
//   IF (EMPTYT7.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD7   DD DSN=W476.W476S7.W476SE7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//*                                                                             
//EMPTYT8 EXEC WEMPTST,DSIN=W476.W476S7.W476CH8(+1)                             
//*                                                                             
//   IF (EMPTYT8.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD8   DD DSN=W476.W476S7.W476CH8(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT9 EXEC WEMPTST,DSIN=W476.W476S7.W476UK7(+1)                             
//*                                                                             
//   IF (EMPTYT9.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD9   DD DSN=W476.W476S7.W476UK7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT10 EXEC WEMPTST,DSIN=W476.W476S7.W476CA7(+1)                            
//*                                                                             
//   IF (EMPTYT10.T.RC = 4) THEN                                                
//      EXEC PGM=IEFBR14                                                        
//DD10  DD DSN=W476.W476S7.W476CA7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT11 EXEC WEMPTST,DSIN=W476.W476S7.W476US8(+1)                            
//*                                                                             
//   IF (EMPTYT11.T.RC = 4) THEN                                                
//      EXEC PGM=IEFBR14                                                        
//DD11  DD DSN=W476.W476S7.W476US8(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT12 EXEC WEMPTST,DSIN=W476.W476S7.W476IR7(+1)                            
//*                                                                             
//   IF (EMPTYT12.T.RC = 4) THEN                                                
//      EXEC PGM=IEFBR14                                                        
//DD12  DD DSN=W476.W476S7.W476IR7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//*EMPTYT13 EXEC WEMPTST,DSIN=W476.W476S7.W476RU7(+1)                           
//**                                                                            
//*   IF (EMPTYT13.T.RC = 4) THEN                                               
//*      EXEC PGM=IEFBR14                                                       
//*DD13  DD DSN=W476.W476S7.W476RU7(+1),DISP=(OLD,DELETE)                       
//*   ENDIF                                                                     
//*                                                                             
//EMPTYT14 EXEC WEMPTST,DSIN=W476.W476S7.W476AU7(+1)                            
//*                                                                             
//   IF (EMPTYT14.T.RC = 4) THEN                                                
//      EXEC PGM=IEFBR14                                                        
//DD14  DD DSN=W476.W476S7.W476AU7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT15 EXEC WEMPTST,DSIN=W476.W476S7.W476AU8(+1)                            
//*                                                                             
//   IF (EMPTYT15.T.RC = 4) THEN                                                
//      EXEC PGM=IEFBR14                                                        
//DD15  DD DSN=W476.W476S7.W476AU8(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT16 EXEC WEMPTST,DSIN=W476.W476S7.W476JP7(+1)                            
//*                                                                             
//   IF (EMPTYT16.T.RC = 4) THEN                                                
//      EXEC PGM=IEFBR14                                                        
//DD16  DD DSN=W476.W476S7.W476JP7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT17 EXEC WEMPTST,DSIN=W476.W476S7.W476JP8(+1)                            
//*                                                                             
//   IF (EMPTYT17.T.RC = 4) THEN                                                
//      EXEC PGM=IEFBR14                                                        
//DD17  DD DSN=W476.W476S7.W476JP8(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT18 EXEC WEMPTST,DSIN=W476.W476S7.W476NL7(+1)                            
//*                                                                             
//   IF (EMPTYT18.T.RC = 4) THEN                                                
//      EXEC PGM=IEFBR14                                                        
//DD18  DD DSN=W476.W476S7.W476NL7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT19 EXEC WEMPTST,DSIN=W476.W476S7.W476IN7(+1)                            
//*                                                                             
//   IF (EMPTYT19.T.RC = 4) THEN                                                
//      EXEC PGM=IEFBR14                                                        
//DD19  DD DSN=W476.W476S7.W476IN7(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J118                                         
