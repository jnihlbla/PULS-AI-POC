//W271J341 JOB (670W2710100W271J341,W100),'RTN W271DA',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* DELAY THE EXECUTION TO AVOID ABEND '777'                                    
//WAIT    EXEC WWAIT,SECONDS=90                                                 
//W271    EXEC W271P041,                                                        
//             INDIN=W271.W271DA                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J341                                         
