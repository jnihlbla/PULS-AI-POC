//W611J019 JOB (640W6110100W611J019,W100),'RTN W611V1',                         
//* ÄR UPPDRAGSKODEN OVAN RÄTT?????                                             
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P019                                                         
//*                                                                             
//EMPTYT EXEC WEMPTST,DSIN=W611.W611V1.W6119P(+1)                               
//    IF (EMPTYT.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DDT    DD DSN=W611.W611V1.W6119P(+1),DISP=(OLD,DELETE)                        
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J019                                         
