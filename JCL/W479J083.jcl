//W479J083 JOB (640W4790100W479J083,W100),'RTN W479D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W479    EXEC W479P083                                                         
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W479.W479D1.W47983(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//      EXEC WSOP                                                               
        ACTIVATE W479JD83                                                       
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W479.W479D1.W47983(+1),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J083                                         
