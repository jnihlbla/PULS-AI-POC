//W418J120 JOB (670W4180100W418J120,W100),'RTN W418D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W418    EXEC W015P021,                                                        
//             DSIN=W418.W418D1.W01521(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J120                                         
//*                                                                             
