//W271J042 JOB (670W2710100W271J042,W100),'RTN W271V3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P042,CPU=4                                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J042                                         
