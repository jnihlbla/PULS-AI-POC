//W463J058 JOB (640W4630100W463J058,W100),'RTN W463S9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//W463    EXEC W463P058                                                         
//*                                                                             
//*******************************************************************           
//* KOLLAR OM UTFILEN MED FELPOSTER ÄR TOM, I SÅFALL TA BORT UTFILEN*           
//*******************************************************************           
//EMPTYT  EXEC WEMPTST,DSIN=W463.W463S9.W46354(+1)                              
//    IF (EMPTYT.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W463.W463S9.W46354(+1),DISP=(OLD,DELETE)                         
//    ELSE                                                                      
//      EXEC WSOP                                                               
        ORDER W463J059                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J058                                         
