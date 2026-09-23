//W440J132 JOB (670W4400100W440J132,W100),'RTN W440S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* DELAY THE EXECUTION TO AVOID ABEND '777'                                    
//WAIT    EXEC WWAIT,SECONDS=60                                                 
//W440    EXEC W440P132                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J132                                         
