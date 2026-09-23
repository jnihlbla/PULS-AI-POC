//W980J090 JOB (540W0090100W980J090,W100),'RTN W980V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST9                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W980     EXEC W980P090                                                        
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W980.W980V1.W98032A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//      EXEC WZ14PDAP,DSIN=W980.W980V1.W98032A(+1)                              
//SYSIN           DD *                                                          
W98090-001                                                                      
W98090                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W980.W980V1.W98032B(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
//      EXEC WZ14PDAP,DSIN=W980.W980V1.W98032B(+1)                              
//SYSIN           DD *                                                          
W98090-002                                                                      
W98090                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY3 EXEC WEMPTST,DSIN=W980.W980V1.W98032C(+1)                              
//    IF (EMPTY3.T.RC = 0) THEN                                                 
//      EXEC WZ14PDAP,DSIN=W980.W980V1.W98032C(+1)                              
//SYSIN           DD *                                                          
W98090-003                                                                      
W98090                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980J090                                         
