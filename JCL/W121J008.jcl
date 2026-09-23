//W121J008 JOB (650W1210100W121J008,W100),'RTN W121PV',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W121    EXEC W121P008                                                         
//EMPTY1 EXEC WEMPTST,DSIN=W121.W121PV.W12108(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W121.W121PV.W12108(+1)                                    
//SYSIN           DD *                                                          
W12108-001                                                                      
W12108                                                                          
//    ENDIF                                                                     
//SOP     EXEC WSOPEND,PROCESS=W121J008                                         
