//W371J09D JOB (640W3710100W371J09D,W100),'RTN W371Y1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W371    EXEC W371P09D                                                         
//*                                                                             
//EMPTY1  EXEC WEMPTST,DSIN=W371.W371Y1.W3719D(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371Y1.W3719D(+1)                                    
//SYSIN           DD *                                                          
W3719D-001                                                                      
W3719D                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J09D                                         
