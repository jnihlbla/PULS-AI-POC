//W612J068 JOB (670W6120100W612J068,W100),'RTN W612V3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
//      INCLUDE MEMBER=DESTN                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W612    EXEC W612P068                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W612.W612V3.DC61.W61268(+1)                          
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W612.W612V3.DC61.W61268(+1)                               
//SYSIN           DD *                                                          
W61268-061                                                                      
W61268                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W612.W612V3.W61268(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W612.W612V3.W61268(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W612.W612V3.W61269(+1)                               
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W612.W612V3.W61269(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY4 EXEC WEMPTST,DSIN=W612.W612V3.DC11.W61268(+1)                          
//    IF (EMPTY4.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W612.W612V3.DC11.W61268(+1)                               
//SYSIN           DD *                                                          
W61268-011                                                                      
W61268                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J068                                         
