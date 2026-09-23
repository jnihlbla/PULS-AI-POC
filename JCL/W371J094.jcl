//W371J094 JOB (640W3710100W371J094,W100),'RTN W371P3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W371    EXEC W371P094                                                         
//*                                                                             
//EMPTY1  EXEC WEMPTST,DSIN=W371.W371P3.W37194(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371P3.W37194(+1)                                    
//SYSIN           DD *                                                          
W37194-001                                                                      
W37194                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J094                                         
