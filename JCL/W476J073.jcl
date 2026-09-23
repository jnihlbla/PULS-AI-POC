//W476J073 JOB (650W4760100W476J073,W100),'RTN W476S8',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
//*                                                                             
//ETSTA   EXEC WEMPTST,DSIN=W476.W476S8.W476L1(+0)                              
//*                                                                             
// IF (ETSTA.T.RC = 0) THEN                                                     
//*                                                                             
// EXEC WZ14DAP2,DSIN=W476.W476S8.W476L1(+0)                                    
//SYSIN           DD *                                                          
W47673-001                                                                      
W47673                                                                          
// ENDIF                                                                        
//*                                                                             
//ETSTB   EXEC WEMPTST,DSIN=W476.W476S8.W476L2(+0)                              
//*                                                                             
// IF (ETSTB.T.RC = 0) THEN                                                     
//*                                                                             
// EXEC WZ14DAP2,DSIN=W476.W476S8.W476L2(+0)                                    
//SYSIN           DD *                                                          
W47673-001                                                                      
W47673                                                                          
//*                                                                             
// ENDIF                                                                        
//*                                                                             
//ETSTC   EXEC WEMPTST,DSIN=W476.W476S8.W476L3(+0)                              
//*                                                                             
// IF (ETSTC.T.RC = 0) THEN                                                     
//*                                                                             
// EXEC WZ14DAP2,DSIN=W476.W476S8.W476L3(+0)                                    
//SYSIN           DD *                                                          
W47673-001                                                                      
W47673                                                                          
//*                                                                             
// ENDIF                                                                        
//*                                                                             
//ETSTD   EXEC WEMPTST,DSIN=W476.W476S8.W476L4(+0)                              
//*                                                                             
// IF (ETSTD.T.RC = 0) THEN                                                     
//*                                                                             
// EXEC WZ14DAP2,DSIN=W476.W476S8.W476L4(+0)                                    
//SYSIN           DD *                                                          
W47673-001                                                                      
W47673                                                                          
//*                                                                             
// ENDIF                                                                        
//*                                                                             
//ETSTE   EXEC WEMPTST,DSIN=W476.W476S8.W476L5(+0)                              
//*                                                                             
// IF (ETSTE.T.RC = 0) THEN                                                     
//*                                                                             
// EXEC WZ14DAP2,DSIN=W476.W476S8.W476L5(+0)                                    
//SYSIN           DD *                                                          
W47673-001                                                                      
W47673                                                                          
//*                                                                             
// ENDIF                                                                        
//*                                                                             
//ETSTF   EXEC WEMPTST,DSIN=W476.W476S8.W476L6(+0)                              
//*                                                                             
// IF (ETSTF.T.RC = 0) THEN                                                     
//*                                                                             
// EXEC WZ14DAP2,DSIN=W476.W476S8.W476L6(+0)                                    
//SYSIN           DD *                                                          
W47673-001                                                                      
W47673                                                                          
//*                                                                             
// ENDIF                                                                        
//*                                                                             
//ETSTG   EXEC WEMPTST,DSIN=W476.W476S8.W476L7(+0)                              
//*                                                                             
// IF (ETSTG.T.RC = 0) THEN                                                     
//*                                                                             
// EXEC WZ14DAP2,DSIN=W476.W476S8.W476L7(+0)                                    
//SYSIN           DD *                                                          
W47673-001                                                                      
W47673                                                                          
//*                                                                             
// ENDIF                                                                        
//*                                                                             
//ETSTH   EXEC WEMPTST,DSIN=W476.W476S8.W476L8(+0)                              
//*                                                                             
// IF (ETSTH.T.RC = 0) THEN                                                     
//*                                                                             
// EXEC WZ14DAP2,DSIN=W476.W476S8.W476L8(+0)                                    
//SYSIN           DD *                                                          
W47673-001                                                                      
W47673                                                                          
//*                                                                             
// ENDIF                                                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W476J073                                         
