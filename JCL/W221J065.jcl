//W221J065 JOB (640W2210100W221J065,W100),'RTN W221S4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W221    EXEC W221P065                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W221.W221S4.W22165(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W221.W221S4.W22165(+1)                                    
//SYSIN           DD *                                                          
W22165-001                                                                      
W22165                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J065                                         
