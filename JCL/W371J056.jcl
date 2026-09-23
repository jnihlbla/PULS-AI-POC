//W371J056 JOB (670W3710100W371J056,W100),'RTN W371S4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W371    EXEC W371P056                                                         
//*                                                                             
&IDFAKT                                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W371.W371S4.W37161(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371S4.W37161(+1)                                    
//SYSIN           DD *                                                          
W37156-001                                                                      
W37156                                                                          
//    ENDIF                                                                     
//EMPTY2 EXEC WEMPTST,DSIN=W371.W371S4.W37162(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371S4.W37162(+1)                                    
//SYSIN           DD *                                                          
W37156-061                                                                      
W37156                                                                          
//    ENDIF                                                                     
//EMPTY3 EXEC WEMPTST,DSIN=W371.W371S4.W37163(+1)                               
//    IF (EMPTY3.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371S4.W37163(+1)                                    
//SYSIN           DD *                                                          
W37156-062                                                                      
W37156                                                                          
//    ENDIF                                                                     
//EMPTY4 EXEC WEMPTST,DSIN=W371.W371S4.W37164(+1)                               
//    IF (EMPTY4.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371S4.W37164(+1)                                    
//SYSIN           DD *                                                          
CORE-PROFORMA                                                                   
&IDDC.&IDUSER.                                                                  
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W371J056                                         
