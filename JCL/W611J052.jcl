//W611J052 JOB (640W6110100W611J052,W100),'RTN W611D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W611    EXEC W611P052                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W611.W611D1.W61152(+1)                                    
//SYSIN           DD *                                                          
W61152-001                                                                      
W61152                                                                          
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W611.W611D1.W61157(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W611.W611D1.W61157(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J052                                         
