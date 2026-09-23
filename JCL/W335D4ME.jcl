//W335D4ME JOB (650W3350100W335D4ME,W100),'RTN W335D4',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//ENV  INCLUDE MEMBER=SYSTZ                                                     
//ENV  INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//******** DAP  MARKNADSBOLAG A SVERIGE                                         
//EMPTY1 EXEC WEMPTST,DSIN=W335.W335D4.W33512(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W335.W335D4.W33512(+0)                                    
//SYSIN           DD *                                                          
W33510-001                                                                      
W33510A                                                                         
//    ENDIF                                                                     
//*                                                                             
//******** DAP  MARKNADSBOLAG B VCEM EUROPA                                     
//EMPTY2 EXEC WEMPTST,DSIN=W335.W335D4.W33513(+0)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W335.W335D4.W33513(+0)                                    
//SYSIN           DD *                                                          
W33510-001                                                                      
W33510B                                                                         
//    ENDIF                                                                     
//*                                                                             
//******** DAP  MARKNADSBOLAG C ASIA PACIFIC                                    
//EMPTY3 EXEC WEMPTST,DSIN=W335.W335D4.W33514(+0)                               
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W335.W335D4.W33514(+0)                                    
//SYSIN           DD *                                                          
W33510-001                                                                      
W33510C                                                                         
//    ENDIF                                                                     
//*                                                                             
//******** DAP  MARKNADSBOLAG E VCNA                                            
//EMPTY4 EXEC WEMPTST,DSIN=W335.W335D4.W33516(+0)                               
//    IF (EMPTY4.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W335.W335D4.W33516(+0)                                    
//SYSIN           DD *                                                          
W33510-001                                                                      
W33510E                                                                         
//    ENDIF                                                                     
//*                                                                             
//******** DAP  MARKNADSBOLAG F VCAS                                            
//EMPTY5 EXEC WEMPTST,DSIN=W335.W335D4.W33517(+0)                               
//    IF (EMPTY5.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W335.W335D4.W33517(+0)                                    
//SYSIN           DD *                                                          
W33510-001                                                                      
W33510F                                                                         
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W335D4ME                                         
