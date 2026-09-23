//W612J088 JOB (640W6120100W612J088,W100),'RTN W612R4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P088                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W612.W612R4.W61289(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W612.W612R4.W61289(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W612.W612R4.W6128A(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W612.W612R4.W6128A(+1)                                    
//SYSIN           DD *                                                          
W61288-011                                                                      
W61288                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J088                                         
