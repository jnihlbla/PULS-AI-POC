//W218J004 JOB (670W2180100W218J004,W100),'RTN W218S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W218    EXEC W218P004                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W218J004                                         
