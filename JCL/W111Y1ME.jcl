//W111Y1ME JOB (670W1110100W111Y1ME,W100),'RTN W111Y1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W111.W111Y1.W1116A(+0)                               
//*                                                                             
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W111.W111Y1.W1116A(+0)                                    
//SYSIN           DD *                                                          
W11168-001                                                                      
W11168                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W111.W111Y1.W1116B(+0)                               
//*                                                                             
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W111.W111Y1.W1116B(+0)                                    
//SYSIN           DD *                                                          
W11168-002                                                                      
W11168                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W111.W111Y1.W1116C(+0)                               
//*                                                                             
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W111.W111Y1.W1116C(+0)                                    
//SYSIN           DD *                                                          
W11168-003                                                                      
W11168                                                                          
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111Y1ME                                         
