//W426JD55 JOB (640W4260100W426JD55,W100),'RTN W426D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
// EXEC WZ14DAP4,DSIN=W426.W426D2.W42655(+0)                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426JD55                                         
