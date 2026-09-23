//W372J053 JOB (640W3710100W372J053,W100),'RTN W371V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W372    EXEC W372P053                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W371.W371V2.W37253(+1)                                    
//SYSIN           DD *                                                          
W37253-091                                                                      
W3725300                                                                        
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W371.W371V2.W37253.WEBDC(+1)                         
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W371.W371V2.W37253.WEBDC(+1)                              
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W372J053                                         
