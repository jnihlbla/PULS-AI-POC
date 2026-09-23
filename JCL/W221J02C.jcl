//W221J02C JOB (670W2210100W221J02C,W100),'RTN W221V3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*                                                                             
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W221     EXEC W221P02C                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J02C                                         
