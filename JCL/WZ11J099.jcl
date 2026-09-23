//WZ11J099 JOB (640WZ110100WZ11J099,W100),'RTN WZ11B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* This is a place holder job that will used for testing                       
//* technical changes.                                                          
//* Should always modify by creating a Temp JCL to run appropriate              
//* programs or procedures.                                                     
//*                                                                             
//*WZ11    EXEC WZ11P099                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WZ11J099                                         
