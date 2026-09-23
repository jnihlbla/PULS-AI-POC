//W476J068 JOB (670W4760100W476J068,W100),'RTN W476S5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W476P068                                                         
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W476.W476S5.W4766AR(+1)                             
//*                                                                             
//   IF (EMPTYT1.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W476.W476S5.W4766AR(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//EMPTYT2 EXEC WEMPTST,DSIN=W476.W476S5.W4766AS(+1)                             
//*                                                                             
//   IF (EMPTYT2.T.RC = 4) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD2   DD DSN=W476.W476S5.W4766AS(+1),DISP=(OLD,DELETE)                        
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J068                                         
