//W612J047 JOB (670W6120100W612J047,W100),'RTN W612D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P047                                                         
//*                                                                             
//EMPTYT EXEC WEMPTST,DSIN=W612.W612D6.W6124S(+1)                               
//    IF (EMPTYT.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DDT    DD DSN=W612.W612D6.W6124S(+1),DISP=(OLD,DELETE)                        
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J047                                         
