//W216J003 JOB (640W2160100W216J003,W100),'RTN W216S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W216.W216S1.W21621(+0)                               
//*                                                                             
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//      EXEC WZ14DAP4,DSIN=W216.W216S1.W21621(+0)                               
//SYSIN           DD *                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W216.W216S1.W21622(+0)                               
//*                                                                             
//    IF (EMPTY2.T.RC = 0) THEN                                                 
//      EXEC WZ14DAP4,DSIN=W216.W216S1.W21622(+0)                               
//SYSIN           DD *                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W216J003                                         
