//W116J07D JOB (670W1160100W116J07D,W100),'RTN W116D4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W116     EXEC W116P07D                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J07D                                         
