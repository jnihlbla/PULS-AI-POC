//W371J090 JOB (650W3710100W371J090,W100),'RTN W371S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*    USER=&IDUSER-OREG                                                        
//*    DISTR=&IDDISTR                                                           
//*                                                                             
//W371     EXEC W371P090                                                        
//W37190.W37190D1 DD *                                                          
&IDUSER-OREG                                                                    
&IDDISTR                                                                        
//*                                                                             
//EMPTY1  EXEC WEMPTST,DSIN=W371.W371S1.W37191(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371S1.W37191(+1)                                    
//SYSIN           DD *                                                          
W37190-001                                                                      
&IDDISTR                                                                        
//    ENDIF                                                                     
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W371J090                                            
