//W115J061 JOB (670W1150100W115J061,W100),'RTN W115S4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W115     EXEC W115P061                                                        
//W11561.W11561D1 DD *                                                          
&PARM.                                                                          
//*                                                                             
//   IF W115.W11561.RC=0 THEN                                                   
//MAIL1  EXEC WMAILSND                                                          
)SEND                                                                           
TITLE  1155 SCREEN-BATCH SUCCESFUL                                              
TO     &MAIL                                                                    
ATTACH W115.W115S4.W11561(+1) W11561.XLS TEXT                                   
MAIL                                                                            
 PLANNER ID UPDATED SUCCESFULLY. CHECK THE ATTACHED FILE FOR DETAILS            
 OF PARTS UPDATED.                                                              
)END                                                                            
//   ELSE                                                                       
//MAIL2  EXEC WMAILSND                                                          
)SEND                                                                           
TITLE  1155 SCREEN-BATCH UNSUCCESFUL                                            
TO     &MAIL                                                                    
ATTACH W115.W115S4.W11561(+1) W11561.XLS TEXT                                   
MAIL                                                                            
 PLANNER ID NOT UPDATED DUE TO ERROR. CHECK THE ATTACHED FILE FOR               
 DETAILS OF ERROR.                                                              
)END                                                                            
//   ENDIF                                                                      
//SOPEND  EXEC WSOPEND,PROCESS=W115J061                                         
