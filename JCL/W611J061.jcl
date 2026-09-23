//W611J061 JOB (670W6110100W611J061,W100),'RTN W611S5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*                                    CNTL  WMEMOSND,EXC                       
//W611    EXEC W611P061                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J061                                         
