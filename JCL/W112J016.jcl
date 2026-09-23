//W112J016 JOB (670W1120100W112J016,W100),'RTN W416V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W112    EXEC W112P016                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W112J016                                         
