//W910J091 JOB (670W2710100W910J091,W100),'RTN W910B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W91091  EXEC W910P091,                                                        
//             INDIN=W910.W910B1,                                               
//             INDUT=W910.W910B1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910J091                                         
