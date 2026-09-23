//W611J674 JOB (640W6110100W611J674,W100),'RTN W611D5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//W611    EXEC W611P674                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W611.W611D5.W61174A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W611.W611D5.W61174A(+1)                                   
//SYSIN           DD *                                                          
W61174-004                                                                      
W61174                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W611.W611D5.W61174I(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W611.W611D5.W61174I(+1)                                   
//SYSIN           DD *                                                          
W61174-006                                                                      
W61174                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W611.W611D5.W61174D(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W611.W611D5.W61174D(+1)                                   
//SYSIN           DD *                                                          
W61174-008                                                                      
W61174                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J674                                         
