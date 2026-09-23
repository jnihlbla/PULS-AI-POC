//W612J074 JOB (640W6120100W612J074,W100),'RTN W612V3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST6                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P074                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W612.W612V3.DC61.W61274(+1)                          
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W612.W612V3.DC61.W61274(+1)                               
//SYSIN           DD *                                                          
W61274-061                                                                      
W61274                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W612.W612V3.DC62.W61274(+1)                          
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W612.W612V3.DC62.W61274(+1)                               
//SYSIN           DD *                                                          
W61274-062                                                                      
W61274                                                                          
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W612J074                                         
