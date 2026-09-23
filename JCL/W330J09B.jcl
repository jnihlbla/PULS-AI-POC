//W330J09B JOB (650W3300100W330J09B,W100),'RTN W330S1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V,TIME=(10,0)                                              
/*JOBPARM LINES=999,CARDS=0,FORMS=1800,LINECT=0                                 
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//*                                                                             
//* DATA=&DATA                                                                  
//* USER=&IDUSER                                                                
//*                                                                             
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P09B EXEC W330P09B                                                        
//W3309B.W3309BD1 DD *                                                          
&DATA                                                                           
//*                                                                             
//EMPTY1  EXEC WEMPTST,DSIN=W330.W330S1.W3309B(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W330.W330S1.W3309B(+1)                                    
//SYSIN           DD *                                                          
W3309B-001                                                                      
&IDUSER                                                                         
//    ENDIF                                                                     
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J09B                                            
