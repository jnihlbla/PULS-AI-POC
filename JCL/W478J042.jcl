//W478J042 JOB (650W2110100W478J042,W100),'RTN W478V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W478    EXEC W478P042                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W478.W478V1.DC61.W47842(+1)                          
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W478.W478V1.DC61.W47842(+1)                               
//SYSIN           DD *                                                          
W47842-061                                                                      
W47842                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W478J042                                         
