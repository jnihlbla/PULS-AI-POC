//W231J056 JOB (650W2310100W231J056,W100),'RTN W231V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W231    EXEC W231P056                                                         
//W23155.EZTVFM DD DATACLAS=PSEN                                                
//SORTWK01 DD  SPACE=(CYL,(400,100),RLSE)                                       
//SORTWK02 DD  SPACE=(CYL,(400,100),RLSE)                                       
//SORTWK03 DD  SPACE=(CYL,(400,100),RLSE)                                       
//SORTWK04 DD  SPACE=(CYL,(400,100),RLSE)                                       
//SORTWK05 DD  SPACE=(CYL,(400,100),RLSE)                                       
//SORTWK06 DD  SPACE=(CYL,(400,100),RLSE)                                       
//*                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W231.W231V1.W2315A(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315A(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT00                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1M2                                          
//*SEND DD DSN=W231.W231V1.W2315B(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W231.W231V1.W2315B(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315B(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT01                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1M3                                          
//*SEND DD DSN=W231.W231V1.W2315C(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W231.W231V1.W2315C(+1)                               
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315C(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT02                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1M4                                          
//*SEND DD DSN=W231.W231V1.W2315D(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY4 EXEC WEMPTST,DSIN=W231.W231V1.W2315D(+1)                               
//    IF (EMPTY4.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315D(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT03                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1M5                                          
//*SEND DD DSN=W231.W231V1.W2315E(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY5 EXEC WEMPTST,DSIN=W231.W231V1.W2315E(+1)                               
//    IF (EMPTY5.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315E(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT04                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1M6                                          
//*SEND DD DSN=W231.W231V1.W2315F(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY6 EXEC WEMPTST,DSIN=W231.W231V1.W2315F(+1)                               
//    IF (EMPTY6.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315F(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT05                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1M7                                          
//*SEND DD DSN=W231.W231V1.W2315G(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY7 EXEC WEMPTST,DSIN=W231.W231V1.W2315G(+1)                               
//    IF (EMPTY7.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315G(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT06                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1M8                                          
//*SEND DD DSN=W231.W231V1.W2315H(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY8 EXEC WEMPTST,DSIN=W231.W231V1.W2315H(+1)                               
//    IF (EMPTY8.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315H(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT07                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1M9                                          
//*SEND DD DSN=W231.W231V1.W2315J(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY9 EXEC WEMPTST,DSIN=W231.W231V1.W2315J(+1)                               
//    IF (EMPTY9.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315J(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT08                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1MA                                          
//*SEND DD DSN=W231.W231V1.W2315K(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY10 EXEC WEMPTST,DSIN=W231.W231V1.W2315K(+1)                              
//    IF (EMPTY10.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315K(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT09                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1MB                                          
//*SEND DD DSN=W231.W231V1.W2315T(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY11 EXEC WEMPTST,DSIN=W231.W231V1.W2315T(+1)                              
//    IF (EMPTY11.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315T(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT10                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1MC                                          
//*SEND DD DSN=W231.W231V1.W2315U(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY12 EXEC WEMPTST,DSIN=W231.W231V1.W2315U(+1)                              
//    IF (EMPTY12.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315U(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT11                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//MEMOSND EXEC WMEMOSND,REQS=W231V1MD                                           
//SEND DD DSN=W231.W231V1.W2315R(+1),DISP=(OLD,KEEP)                            
//*                                                                             
//EMPTY13 EXEC WEMPTST,DSIN=W231.W231V1.W2315R(+1)                              
//    IF (EMPTY13.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315R(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT12                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//MEMOSND EXEC WMEMOSND,REQS=W231V1ME                                           
//SEND DD DSN=W231.W231V1.W2315L(+1),DISP=(OLD,KEEP)                            
//*                                                                             
//EMPTY14 EXEC WEMPTST,DSIN=W231.W231V1.W2315L(+1)                              
//    IF (EMPTY14.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315L(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT13                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//*MEMOSND EXEC WMEMOSND,REQS=W231V1MF                                          
//*SEND DD DSN=W231.W231V1.W2315M(+1),DISP=(OLD,KEEP)                           
//*                                                                             
//EMPTY15 EXEC WEMPTST,DSIN=W231.W231V1.W2315M(+1)                              
//    IF (EMPTY15.T.RC = 0) THEN                                                
// EXEC WZ14PDAP,DSIN=W231.W231V1.W2315M(+1)                                    
//SYSIN           DD *                                                          
ANSK-RAPPORT14                                                                  
W23156                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W231J056                                         
