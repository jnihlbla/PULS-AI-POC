//W225J073 JOB (640W2250100W225J073,W100),'RTN W225D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W225    EXEC W225P073                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W225.W225D2.W22573(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W225.W225D2.W22573(+1)                                    
//SYSIN           DD *                                                          
W22573-001                                                                      
W22573                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W225J073                                         
