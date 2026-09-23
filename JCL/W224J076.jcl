//W224J076 JOB (640W2240100W224J076,W100),'RTN W224S4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W224    EXEC W224P076                                                         
//*                                                                             
//* *** ALARM 223 CHINA ***                                                     
//EMPTY1 EXEC WEMPTST,DSIN=W224.W224S4.W2247601(+1)                             
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W224.W224S4.W2247601(+1)                                  
//SYSIN           DD *                                                          
W22476-001                                                                      
W22476                                                                          
//    ENDIF                                                                     
//*                                                                             
//* *** ALARM 223 NA-USA ***                                                    
//EMPTY2 EXEC WEMPTST,DSIN=W224.W224S4.W2247602(+1)                             
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W224.W224S4.W2247602(+1)                                  
//SYSIN           DD *                                                          
W22476-002                                                                      
W22476                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J076                                         
