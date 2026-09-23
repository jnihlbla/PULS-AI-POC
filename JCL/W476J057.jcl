//W476J057 JOB (670W4760100W476J057,W100),'RTN W476D5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W476P057                                                         
//*                                                                             
//********************************************************************          
//*                                                                  *          
//*    OM FIL W47657 INTE ÄR TOM SKICKAS ETT MEMO TILL MOTTAGARE     *          
//*    SOM DEFINIERATS I DAP                                         *          
//*                                                                  *          
//********************************************************************          
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W476.W476D5.W47657(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W476.W476D5.W47657(+1)                                    
//SYSIN           DD *                                                          
W47657-001                                                                      
W47657                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J057                                         
