//W413J120 JOB (670W4130100W413J120,W100),'RTN W413M1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W413    EXEC W413P120                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W413J120                                         
