//W513J018 JOB (640W5130100W513J018,W100),'RTN W513R1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W513    EXEC W513P018                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W513.W513R1.W51318(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513R1.W51318(+1)                                    
//SYSIN           DD *                                                          
W51318-001                                                                      
W51318                                                                          
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W513J018                                         
