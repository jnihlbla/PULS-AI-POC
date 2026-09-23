//W371J057 JOB (640W3710100W371J157,W100),'RTN W371V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*ANVÄNDER SAMMA PROCEDUR SOM J057 I RUTIN W371V2                              
//W371    EXEC W371P057,                                                        
//             INDIN=W371.W371V2                                                
//*                                                                             
// EXEC WZ14PDAP,DSIN=W371.W371V2.W37157(+1)                                    
//SYSIN           DD *                                                          
W37157-091                                                                      
W3715700                                                                        
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W371.W371V2.W37157.WEBDC(+1)                         
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W371.W371V2.W37157.WEBDC(+1)                              
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J057                                         
