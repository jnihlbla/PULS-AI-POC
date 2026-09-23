//W371J040 JOB (640W3710100W371J040,W100),'RTN W371M1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W371    EXEC W371P040                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W371.W371M1.W37140(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371M1.W37140(+1)                                    
//SYSIN           DD *                                                          
W37140-001                                                                      
W3714000                                                                        
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J040                                         
