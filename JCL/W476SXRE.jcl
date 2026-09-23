//W476SXRE JOB (670W4760100W476SXRE,W100),'RTN W476SX',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W476.W476SX.W47661(+0)                              
//*                                                                             
//    IF (EMPTYT1.T.RC = 0) THEN                                                
//      EXEC WSOP                                                               
        ORDER W476S5                                                            
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W476.W476SX.W47661(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476SXRE                                         
