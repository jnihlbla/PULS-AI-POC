//W611J053 JOB (640W6110100W611J053,W100),'RTN W611V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST6                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* OM INGA SORTWORKAR I PROCEDUREN TA BORT SORTALLOKERINGEN NEDAN              
//SORT    EXEC SORT004M                                                         
//W611    EXEC W611P053                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W611.W611V1.W61153(+1)                                    
//SYSIN           DD *                                                          
W61153-001                                                                      
W61153                                                                          
// EXEC WZ14PDAP,DSIN=W611.W611V1.W61156(+1)                                    
//SYSIN           DD *                                                          
W61153-002                                                                      
W61153                                                                          
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W611.W611V1.W61157(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W611.W611V1.W61157(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W611.W611V1.W61158(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W611.W611V1.W61158(+1)                                    
//SYSIN           DD *                                                          
W61153-004                                                                      
CN                                                                              
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J053                                         
