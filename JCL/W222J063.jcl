//W222J063 JOB (640W2220100W222J063,W100),'RTN W222V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W222    EXEC W222P063                                                         
//*                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W222.W222V1.W22263A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W222.W222V1.W22263A(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W222.W222V1.W22263B(+1)                              
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W222.W222V1.W22263B(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W222J063                                         
