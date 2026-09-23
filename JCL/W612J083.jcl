//W612J083 JOB (640W6120100W612J083,W100),'RTN W612V4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W612    EXEC W612P083                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W612.W612V4.W61283(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W612.W612V4.W61283(+1)                                    
//SYSIN           DD *                                                          
W61283-001                                                                      
CN                                                                              
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W612.W612V4.W61284(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W612.W612V4.W61284(+1)                                    
//SYSIN           DD *                                                          
W61283-002                                                                      
CN                                                                              
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J083                                         
