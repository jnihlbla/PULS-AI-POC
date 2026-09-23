//W221J009 JOB (670W2210100W221J009,W100),'RTN W221D6',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W221    EXEC W221P009                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J009                                         
