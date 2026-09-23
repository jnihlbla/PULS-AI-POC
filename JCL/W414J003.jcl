//W414J003 JOB (640W4120100W414J003,W100),'RTN W414D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W414    EXEC W414P003                                                         
//*                                                                             
//EMPTYT EXEC WEMPTST,DSIN=W414.W414D1.W4140S(+1)                               
//    IF (EMPTYT.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DDT    DD DSN=W414.W414D1.W4140S(+1),DISP=(OLD,DELETE)                        
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W414J003                                         
