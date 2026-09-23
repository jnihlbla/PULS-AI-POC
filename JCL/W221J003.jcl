//W221J003 JOB (670W2210100W221J004,W100),'RTN W221S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W221    EXEC W221P003                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J003                                         
