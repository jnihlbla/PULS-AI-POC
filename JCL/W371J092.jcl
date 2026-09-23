//W371J092 JOB (650W3710100W371J092,W100),'RTN W371S1',                         
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
//W371     EXEC W371P092                                                        
//W37192.W37192D1 DD *                                                          
&IDUSER-OREG                                                                    
&IDDISTR                                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W371J092                                            
