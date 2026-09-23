//W463J061 JOB (640W4630100W463J061,W100),'RTN W463D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W463    EXEC W463P061                                                         
//*                                                                             
//*******************************************************************           
//* OM RETURKOD=8 PASSIVERA RESTEN OCH SKICKA MEMO                  *           
//*******************************************************************           
// IF W463.W46361.RC = 8 THEN                                                   
//SKIP1   EXEC WSOP                                                             
 PASSIVATE W463D6                                                               
//*                                                                             
//MEMO1  EXEC WMEMOSND                                                          
//M.APIFILE  DD *                                                               
)SEND                                                                           
TITLE WRONG INPUT FILE W463D6                                                   
OPTION FORCE                                                                    
DEST WSYST@VOLVOCARS.COM                                                        
MEMO                                                                            
                                                                                
  FELAKTIG FIL FRÅN IMS ADVANSYS.                                               
  FELAKTIGA FILER PÅ W46364.                                                    
  RUTINEN AUTOMATISKT AVSLUTAD.                                                 
  MEDDELA SYSTEMANSVARIG!!                                                      
                                                                                
)END                                                                            
//M.SEND     DD DUMMY                                                           
// ENDIF                                                                        
//*                                                                             
//*******************************************************************           
//* KOLLAR OM W46364 MED FELPOSTER ÄR TOM, I SÅFALL TA BORT UTFILEN *           
//*******************************************************************           
//EMPTYT  EXEC WEMPTST,DSIN=W463.W463D6.W46364(+1)                              
//    IF (EMPTYT.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W463.W463D6.W46364(+1),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J061                                         
