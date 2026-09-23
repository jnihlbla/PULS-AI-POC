//W271J137 JOB (670W2710100W271J137,W100),'RTN W271D9',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTZ                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W271.W271D9.W27137(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W271.W271D9.W27137(+0)                                    
//SYSIN           DD *                                                          
W27137-001                                                                      
W27137                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J137                                         
