//W221J098 JOB (640W2210100W221J098,W100),'RTN W221V3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND  IMG0                                                              
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W221    EXEC W221P098                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W221.W221V3.W22198(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W221.W221V3.W22198(+1)                                    
//SYSIN           DD *                                                          
W22198-001                                                                      
W22198                                                                          
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W221J098                                         
