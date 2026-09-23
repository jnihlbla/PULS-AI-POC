//W371J05E JOB (640W3710100W371J05E,W100),'RTN W371V3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST8                                                    
//      INCLUDE MEMBER=SYST3                                                    
//*                                                                             
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W371    EXEC W371P05E                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W371.W371V3.W3715E(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W371.W371V3.W3715E(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J05E                                         
