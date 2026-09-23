//W371J058 JOB (670W3710100W371J058,W100),'RTN W371S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*    USER=&IDUSER                                                             
//*    DC=&IDDC                                                                 
//*    DISTRICT=&IDDISTR                                                        
//*    DC-REC=&IDDC-REC                                                         
//*                                                                             
//W371     EXEC W371P058                                                        
//*                                                                             
//W37158.W37158D1 DD *                                                          
&IDUSER                                                                         
&IDDC                                                                           
&IDDISTR                                                                        
&IDDC-REC                                                                       
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W371.W371S2.W37151(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371S2.W37151(+1)                                    
//SYSIN           DD *                                                          
W37158-001                                                                      
&IDDC                                                                           
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W371.W371S2.W37152(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371S2.W37152(+1)                                    
//SYSIN           DD *                                                          
CORE-EXPORT                                                                     
&IDDC.&IDUSER.                                                                  
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J058                                         
