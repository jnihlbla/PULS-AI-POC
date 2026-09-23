//W613J016 JOB (640W6130100W613J016,W100),'RTN W613R1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W613    EXEC W613P016                                                         
//*                                                                             
//********************************************************************          
//* MAIL TILL VCCS VIA D&P                                                      
//* W613.W613R1.W61317(+1)                                                      
//********************************************************************          
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W613.W613R1.W61317(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W613.W613R1.W61317(+1)                                    
//SYSIN           DD *                                                          
W61316-001                                                                      
W61316                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W613J016                                         
